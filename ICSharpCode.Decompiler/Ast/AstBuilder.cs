// Copyright (c) 2011 AlphaSierraPapa for the SharpDevelop Team
// 
// Permission is hereby granted, free of charge, to any person obtaining a copy of this
// software and associated documentation files (the "Software"), to deal in the Software
// without restriction, including without limitation the rights to use, copy, modify, merge,
// publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons
// to whom the Software is furnished to do so, subject to the following conditions:
// 
// The above copyright notice and this permission notice shall be included in all copies or
// substantial portions of the Software.
// 
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,
// INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR
// PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE
// FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
// OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
// DEALINGS IN THE SOFTWARE.

using System;
using System.Collections.Concurrent;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;
using System.Threading;

using ICSharpCode.Decompiler;
using ICSharpCode.Decompiler.Ast.Transforms;
using ICSharpCode.Decompiler.ILAst;
using ICSharpCode.NRefactory.CSharp;
using ICSharpCode.NRefactory.Utils;
using dnlib.DotNet;
using dnlib.DotNet.Emit;
using ICSharpCode.NRefactory.TypeSystem;

namespace ICSharpCode.Decompiler.Ast
{
	[Flags]
	public enum ConvertTypeOptions
	{
		None = 0,
		IncludeNamespace = 1,
		IncludeTypeParameterDefinitions = 2,
		DoNotUsePrimitiveTypeNames = 4
	}
	
	public class AstBuilder
	{
		DecompilerContext context;
		SyntaxTree syntaxTree = new SyntaxTree();
		Dictionary<string, NamespaceDeclaration> astNamespaces = new Dictionary<string, NamespaceDeclaration>();
		bool transformationsHaveRun;
		
		public AstBuilder(DecompilerContext context)
		{
			if (context == null)
				throw new ArgumentNullException("context");
			this.context = context;
			this.DecompileMethodBodies = true;
		}
		
		public static bool MemberIsHidden(IMemberRef member, DecompilerSettings settings)
		{
			MethodDef method = member as MethodDef;
			if (method != null) {
				if (method.IsGetter || method.IsSetter || method.IsAddOn || method.IsRemoveOn)
					return true;
				if (settings.AnonymousMethods && method.HasGeneratedName() && method.IsCompilerGenerated())
					return true;
			}

			TypeDef type = member as TypeDef;
			if (type != null) {
				if (type.DeclaringType != null) {
					if (settings.AnonymousMethods && IsClosureType(type))
						return true;
					if (settings.YieldReturn && YieldReturnDecompiler.IsCompilerGeneratorEnumerator(type))
						return true;
					if (settings.AsyncAwait && AsyncDecompiler.IsCompilerGeneratedStateMachine(type))
						return true;
				} else if (type.IsCompilerGenerated()) {
					if (type.Name.StartsWith("<PrivateImplementationDetails>", StringComparison.Ordinal))
						return true;
					if (type.IsAnonymousType())
						return true;
				}
			}
			
			FieldDef field = member as FieldDef;
			if (field != null) {
				if (field.IsCompilerGenerated()) {
					if (settings.AnonymousMethods && IsAnonymousMethodCacheField(field))
						return true;
					if (settings.AutomaticProperties && IsAutomaticPropertyBackingField(field))
						return true;
					if (settings.SwitchStatementOnString && IsSwitchOnStringCache(field))
						return true;
				}
				// event-fields are not [CompilerGenerated]
				if (settings.AutomaticEvents && field.DeclaringType.Events.Any(ev => ev.Name == field.Name))
					return true;
			}
			
			return false;
		}

		static bool IsSwitchOnStringCache(FieldDef field)
		{
			return field.Name.StartsWith("<>f__switch", StringComparison.Ordinal);
		}

		static bool IsAutomaticPropertyBackingField(FieldDef field)
		{
			return field.HasGeneratedName() && field.Name.EndsWith("BackingField", StringComparison.Ordinal);
		}

		static bool IsAnonymousMethodCacheField(FieldDef field)
		{
			return field.Name.StartsWith("CS$<>", StringComparison.Ordinal) || field.Name.StartsWith("<>f__am", StringComparison.Ordinal);
		}

		static bool IsClosureType(TypeDef type)
		{
			return type.HasGeneratedName() && type.IsCompilerGenerated() && (type.Name.Contains("DisplayClass") || type.Name.Contains("AnonStorey"));
		}
		
		/// <summary>
		/// Runs the C# transformations on the compilation unit.
		/// </summary>
		public void RunTransformations()
		{
			RunTransformations(null);
		}
		
		public void RunTransformations(Predicate<IAstTransform> transformAbortCondition)
		{
			TransformationPipeline.RunTransformationsUntil(syntaxTree, transformAbortCondition, context);
			transformationsHaveRun = true;
		}
		
		/// <summary>
		/// Gets the abstract source tree.
		/// </summary>
		public SyntaxTree SyntaxTree {
			get { return syntaxTree; }
		}
		
		/// <summary>
		/// Generates C# code from the abstract source tree.
		/// </summary>
		/// <remarks>This method adds ParenthesizedExpressions into the AST, and will run transformations if <see cref="RunTransformations"/> was not called explicitly</remarks>
		public void GenerateCode(ITextOutput output)
		{
			if (!transformationsHaveRun)
				RunTransformations();
			
			syntaxTree.AcceptVisitor(new InsertParenthesesVisitor { InsertParenthesesForReadability = true });
			var outputFormatter = new TextOutputFormatter(output) { FoldBraces = context.Settings.FoldBraces };
			var formattingPolicy = context.Settings.CSharpFormattingOptions;
			syntaxTree.AcceptVisitor(new CSharpOutputVisitor(outputFormatter, formattingPolicy));
		}
		
		public void AddAssembly(AssemblyDef assemblyDefinition, bool onlyAssemblyLevel = false)
		{
			AddAssembly(assemblyDefinition.ManifestModule, onlyAssemblyLevel);
		}
		
		public void AddAssembly(ModuleDef moduleDefinition, bool onlyAssemblyLevel = false)
		{
			if (moduleDefinition.Assembly != null && moduleDefinition.Assembly.Version != null) {
				syntaxTree.AddChild(
					new AttributeSection {
						AttributeTarget = "assembly",
						Attributes = {
							new NRefactory.CSharp.Attribute {
								Type = new SimpleType("AssemblyVersion")
									.WithAnnotation(moduleDefinition.CorLibTypes.GetTypeRef(
										"System.Reflection", "AssemblyVersionAttribute")),
								Arguments = {
									new PrimitiveExpression(moduleDefinition.Assembly.Version.ToString())
								}
							}
						}
					}, EntityDeclaration.AttributeRole);
			}
			
			if (moduleDefinition.Assembly != null) {
				ConvertCustomAttributes(syntaxTree, moduleDefinition.Assembly, "assembly");
				ConvertSecurityAttributes(syntaxTree, moduleDefinition.Assembly, "assembly");
			}
			ConvertCustomAttributes(syntaxTree, moduleDefinition, "module");
			AddTypeForwarderAttributes(syntaxTree, moduleDefinition, "assembly");
			
			if (!onlyAssemblyLevel) {
				foreach (TypeDef typeDef in moduleDefinition.Types) {
					// Skip the <Module> class
					if (typeDef.Name == "<Module>") continue;
					// Skip any hidden types
					if (AstBuilder.MemberIsHidden(typeDef, context.Settings))
						continue;

					AddType(typeDef);
				}
			}
		}
		
		void AddTypeForwarderAttributes(SyntaxTree astCompileUnit, ModuleDef module, string target)
		{
			if (!module.HasExportedTypes)
				return;
			foreach (ExportedType type in module.ExportedTypes) {
				if (type.IsForwarder) {
					var typeRef = new TypeRefUser(module, type.Namespace, type.Namespace, (IResolutionScope)type.Scope);
					var forwardedType = CreateTypeOfExpression(typeRef.ToTypeSig());
					astCompileUnit.AddChild(
						new AttributeSection {
							AttributeTarget = target,
							Attributes = {
								new NRefactory.CSharp.Attribute {
									Type = new SimpleType("TypeForwardedTo")
										.WithAnnotation(module.CorLibTypes.GetTypeRef(
											"System.Runtime.CompilerServices", "TypeForwardedToAttribute")),
									Arguments = { forwardedType }
								}
							}
						}, EntityDeclaration.AttributeRole);
				}
			}
		}
		
		NamespaceDeclaration GetCodeNamespace(string name)
		{
			if (string.IsNullOrEmpty(name)) {
				return null;
			}
			if (astNamespaces.ContainsKey(name)) {
				return astNamespaces[name];
			} else {
				// Create the namespace
				NamespaceDeclaration astNamespace = new NamespaceDeclaration { Name = name };
				syntaxTree.Members.Add(astNamespace);
				astNamespaces[name] = astNamespace;
				return astNamespace;
			}
		}
		
		public void AddType(TypeDef typeDef)
		{
			var astType = CreateType(typeDef);
			NamespaceDeclaration astNS = GetCodeNamespace(typeDef.Namespace);
			if (astNS != null) {
				astNS.Members.Add(astType);
			} else {
				syntaxTree.Members.Add(astType);
			}
		}
		
		public void AddMethod(MethodDef method)
		{
			AstNode node = method.IsConstructor ? (AstNode)CreateConstructor(method) : CreateMethod(method);
			syntaxTree.Members.Add(node);
		}

		public void AddProperty(PropertyDef property)
		{
			syntaxTree.Members.Add(CreateProperty(property));
		}
		
		public void AddField(FieldDef field)
		{
			syntaxTree.Members.Add(CreateField(field));
		}
		
		public void AddEvent(EventDef ev)
		{
			syntaxTree.Members.Add(CreateEvent(ev));
		}
		
		/// <summary>
		/// Creates the AST for a type definition.
		/// </summary>
		/// <param name="typeDef"></param>
		/// <returns>TypeDeclaration or DelegateDeclaration.</returns>
		public EntityDeclaration CreateType(TypeDef typeDef)
		{
			// create type
			TypeDef oldCurrentType = context.CurrentType;
			context.CurrentType = typeDef;
			TypeDeclaration astType = new TypeDeclaration();
			ConvertAttributes(astType, typeDef);
			astType.AddAnnotation(typeDef);
			astType.Modifiers = ConvertModifiers(typeDef);
			astType.Name = CleanName(typeDef.Name);
			
			if (typeDef.IsEnum) {  // NB: Enum is value type
				astType.ClassType = ClassType.Enum;
				astType.Modifiers &= ~Modifiers.Sealed;
			} else if (typeDef.IsValueType) {
				astType.ClassType = ClassType.Struct;
				astType.Modifiers &= ~Modifiers.Sealed;
			} else if (typeDef.IsInterface) {
				astType.ClassType = ClassType.Interface;
				astType.Modifiers &= ~Modifiers.Abstract;
			} else {
				astType.ClassType = ClassType.Class;
			}
			
			IEnumerable<GenericParam> genericParameters = typeDef.GenericParameters;
			if (typeDef.DeclaringType != null && typeDef.DeclaringType.HasGenericParameters)
				genericParameters = genericParameters.Skip(typeDef.DeclaringType.GenericParameters.Count);
			astType.TypeParameters.AddRange(MakeTypeParameters(genericParameters));
			astType.Constraints.AddRange(MakeConstraints(genericParameters));
			
			EntityDeclaration result = astType;
			if (typeDef.IsEnum) {
				long expectedEnumMemberValue = 0;
				bool forcePrintingInitializers = IsFlagsEnum(typeDef);
				foreach (FieldDef field in typeDef.Fields) {
					if (!field.IsStatic) {
						// the value__ field
						if (field.FieldType.ElementType != ElementType.I4) {
							astType.AddChild(ConvertType(field.FieldType), Roles.BaseType);
						}
					} else {
						EnumMemberDeclaration enumMember = new EnumMemberDeclaration();
						enumMember.AddAnnotation(field);
						enumMember.Name = CleanName(field.Name);
						long memberValue = (long)CSharpPrimitiveCast.Cast(TypeCode.Int64, field.Constant.Value, false);
						if (forcePrintingInitializers || memberValue != expectedEnumMemberValue) {
							enumMember.AddChild(new PrimitiveExpression(field.Constant.Value), EnumMemberDeclaration.InitializerRole);
						}
						expectedEnumMemberValue = memberValue + 1;
						astType.AddChild(enumMember, Roles.TypeMemberRole);
					}
				}
			} else if (typeDef.BaseType != null && typeDef.BaseType.FullName == "System.MulticastDelegate") {
				DelegateDeclaration dd = new DelegateDeclaration();
				dd.Modifiers = astType.Modifiers & ~Modifiers.Sealed;
				dd.Name = astType.Name;
				dd.AddAnnotation(typeDef);
				astType.Attributes.MoveTo(dd.Attributes);
				astType.TypeParameters.MoveTo(dd.TypeParameters);
				astType.Constraints.MoveTo(dd.Constraints);
				foreach (var m in typeDef.Methods) {
					if (m.Name == "Invoke") {
						dd.ReturnType = ConvertType(m.ReturnType, m.Parameters.ReturnParameter.ParamDef);
						dd.Parameters.AddRange(MakeParameters(m));
						ConvertAttributes(dd, m.Parameters.ReturnParameter, m.Module);
					}
				}
				result = dd;
			} else {
				// Base type
				if (typeDef.BaseType != null && !typeDef.IsValueType && typeDef.BaseType.FullName != "System.Object") {
					astType.AddChild(ConvertType(typeDef.BaseType), Roles.BaseType);
				}
				foreach (var i in typeDef.Interfaces)
					astType.AddChild(ConvertType(i.Interface), Roles.BaseType);
				
				AddTypeMembers(astType, typeDef);

				if (astType.Members.OfType<IndexerDeclaration>().Any(idx => idx.PrivateImplementationType.IsNull)) {
					// Remove the [DefaultMember] attribute if the class contains indexers
					foreach (AttributeSection section in astType.Attributes) {
						foreach (var attr in section.Attributes) {
							ITypeDefOrRef tr = attr.Type.Annotation<ITypeDefOrRef>();
							if (tr != null && tr.Name == "DefaultMemberAttribute" && tr.Namespace == "System.Reflection") {
								attr.Remove();
							}
						}
						if (section.Attributes.Count == 0)
							section.Remove();
					}
				}
			}

			context.CurrentType = oldCurrentType;
			return result;
		}

		internal static string CleanName(string name)
		{
			int pos = name.LastIndexOf('`');
			if (pos >= 0)
				name = name.Substring(0, pos);
			pos = name.LastIndexOf('.');
			if (pos >= 0)
				name = name.Substring(pos + 1);
			return name;
		}

		#region Create TypeOf Expression
		/// <summary>
		/// Creates a typeof-expression for the specified type.
		/// </summary>
		public static TypeOfExpression CreateTypeOfExpression(TypeSig type)
		{
			return new TypeOfExpression(AddEmptyTypeArgumentsForUnboundGenerics(ConvertType(type)));
		}
		
		static AstType AddEmptyTypeArgumentsForUnboundGenerics(AstType type)
		{
			ITypeDefOrRef typeRef = type.Annotation<ITypeDefOrRef>();
			if (typeRef == null)
				return type;
			TypeDef typeDef = typeRef.ResolveTypeDef(); // need to resolve to figure out the number of type parameters
			if (typeDef == null || !typeDef.HasGenericParameters)
				return type;
			SimpleType sType = type as SimpleType;
			MemberType mType = type as MemberType;
			if (sType != null) {
				while (typeDef.GenericParameters.Count > sType.TypeArguments.Count) {
					sType.TypeArguments.Add(new SimpleType(""));
				}
			}
			
			if (mType != null) {
				AddEmptyTypeArgumentsForUnboundGenerics(mType.Target);
				
				int outerTypeParamCount = typeDef.DeclaringType == null ? 0 : typeDef.DeclaringType.GenericParameters.Count;
				
				while (typeDef.GenericParameters.Count - outerTypeParamCount > mType.TypeArguments.Count) {
					mType.TypeArguments.Add(new SimpleType(""));
				}
			}
			
			return type;
		}
		#endregion
		
		#region Convert Type Reference
		/// <summary>
		/// Converts a type reference.
		/// </summary>
		/// <param name="type">The Cecil type reference that should be converted into
		/// a type system type reference.</param>
		/// <param name="typeAttributes">Attributes associated with the Cecil type reference.
		/// This is used to support the 'dynamic' type.</param>
		public static AstType ConvertType(TypeSig type, IHasCustomAttribute typeAttributes = null, ConvertTypeOptions options = ConvertTypeOptions.None)
		{
			int typeIndex = 0;
			return ConvertType(type, typeAttributes, ref typeIndex, options);
		}

		/// <summary>
		/// Converts a type reference.
		/// </summary>
		/// <param name="type">The Cecil type reference that should be converted into
		/// a type system type reference.</param>
		/// <param name="typeAttributes">Attributes associated with the Cecil type reference.
		/// This is used to support the 'dynamic' type.</param>
		public static AstType ConvertType(ITypeDefOrRef type, IHasCustomAttribute typeAttributes = null, ConvertTypeOptions options = ConvertTypeOptions.None)
		{
			int typeIndex = 0;
			return ConvertType(type, typeAttributes, ref typeIndex, options);
		}

		static AstType ConvertType(TypeSig type, IHasCustomAttribute typeAttributes, ref int typeIndex, ConvertTypeOptions options)
		{
			type = type.RemoveModifiers();
			if (type == null) {
				return AstType.Null;
			}
			
			if (type is ByRefSig) {
				typeIndex++;
				// by reference type cannot be represented in C#; so we'll represent it as a pointer instead
				return ConvertType(type.Next, typeAttributes, ref typeIndex, options)
					.MakePointerType();
			} else if (type is PtrSig) {
				typeIndex++;
				return ConvertType(type.Next, typeAttributes, ref typeIndex, options)
					.MakePointerType();
			} else if (type is ArraySigBase) {
				typeIndex++;
				return ConvertType(type.Next, typeAttributes, ref typeIndex, options)
					.MakeArrayType((int)(type as ArraySigBase).Rank);
			} else if (type is GenericInstSig) {
				GenericInstSig gType = (GenericInstSig)type;
				if (gType.GenericType.Namespace == "System" && gType.GenericType.TypeName == "Nullable`1" && gType.GenericArguments.Count == 1) {
					typeIndex++;
					return new ComposedType {
						BaseType = ConvertType(gType.GenericArguments[0], typeAttributes, ref typeIndex, options),
						HasNullableSpecifier = true
					};
				}
				AstType baseType = ConvertType(gType.GenericType, typeAttributes, ref typeIndex, options & ~ConvertTypeOptions.IncludeTypeParameterDefinitions);
				List<AstType> typeArguments = new List<AstType>();
				foreach (var typeArgument in gType.GenericArguments) {
					typeIndex++;
					typeArguments.Add(ConvertType(typeArgument, typeAttributes, ref typeIndex, options));
				}
				ApplyTypeArgumentsTo(baseType, typeArguments);
				return baseType;
			} else if (type is GenericSig) {
				return new SimpleType(type.TypeName);
			} else if (type is TypeDefOrRefSig) {
				return ConvertType(((TypeDefOrRefSig)type).TypeDefOrRef, typeAttributes, ref typeIndex, options);
			} else {
				throw new NotSupportedException();
			}
		}
		
		static AstType ConvertType(ITypeDefOrRef type, IHasCustomAttribute typeAttributes, ref int typeIndex, ConvertTypeOptions options)
		{
			if (type == null) {
				return AstType.Null;
			} else if (type is TypeSpec) {
				return ConvertType(((TypeSpec)type).TypeSig, typeAttributes, ref typeIndex, options);
			}

			if (type.DeclaringType != null) {
				AstType typeRef = ConvertType(type.DeclaringType, typeAttributes, ref typeIndex, options & ~ConvertTypeOptions.IncludeTypeParameterDefinitions);
				string namepart = ICSharpCode.NRefactory.TypeSystem.ReflectionHelper.SplitTypeParameterCountFromReflectionName(type.Name);
				MemberType memberType = new MemberType { Target = typeRef, MemberName = namepart };
				memberType.AddAnnotation(type);
				if ((options & ConvertTypeOptions.IncludeTypeParameterDefinitions) == ConvertTypeOptions.IncludeTypeParameterDefinitions) {
					AddTypeParameterDefininitionsTo(type, memberType);
				}
				return memberType;
			} else {
				string ns = type.Namespace ?? string.Empty;
				string name = type.Name;
				if (name == null)
					throw new InvalidOperationException("type.Name returned null. Type: " + type.ToString());
				
				if (name == "Object" && ns == "System" && HasDynamicAttribute(typeAttributes, typeIndex)) {
					return new PrimitiveType("dynamic");
				} else {
					if (ns == "System") {
						if ((options & ConvertTypeOptions.DoNotUsePrimitiveTypeNames)
						    != ConvertTypeOptions.DoNotUsePrimitiveTypeNames) {
							switch (name) {
								case "SByte":
									return new PrimitiveType("sbyte").WithAnnotation(type);
								case "Int16":
									return new PrimitiveType("short").WithAnnotation(type);
								case "Int32":
									return new PrimitiveType("int").WithAnnotation(type);
								case "Int64":
									return new PrimitiveType("long").WithAnnotation(type);
								case "Byte":
									return new PrimitiveType("byte").WithAnnotation(type);
								case "UInt16":
									return new PrimitiveType("ushort").WithAnnotation(type);
								case "UInt32":
									return new PrimitiveType("uint").WithAnnotation(type);
								case "UInt64":
									return new PrimitiveType("ulong").WithAnnotation(type);
								case "String":
									return new PrimitiveType("string").WithAnnotation(type);
								case "Single":
									return new PrimitiveType("float").WithAnnotation(type);
								case "Double":
									return new PrimitiveType("double").WithAnnotation(type);
								case "Decimal":
									return new PrimitiveType("decimal").WithAnnotation(type);
								case "Char":
									return new PrimitiveType("char").WithAnnotation(type);
								case "Boolean":
									return new PrimitiveType("bool").WithAnnotation(type);
								case "Void":
									return new PrimitiveType("void").WithAnnotation(type);
								case "Object":
									return new PrimitiveType("object").WithAnnotation(type);
							}
						}
					}
					
					name = ICSharpCode.NRefactory.TypeSystem.ReflectionHelper.SplitTypeParameterCountFromReflectionName(name);
					
					AstType astType;
					if ((options & ConvertTypeOptions.IncludeNamespace) == ConvertTypeOptions.IncludeNamespace && ns.Length > 0) {
						string[] parts = ns.Split('.');
						AstType nsType = new SimpleType(parts[0]);
						for (int i = 1; i < parts.Length; i++) {
							nsType = new MemberType { Target = nsType, MemberName = parts[i] };
						}
						astType = new MemberType { Target = nsType, MemberName = name };
					} else {
						astType = new SimpleType(name);
					}
					astType.AddAnnotation(type);
					
					if ((options & ConvertTypeOptions.IncludeTypeParameterDefinitions) == ConvertTypeOptions.IncludeTypeParameterDefinitions) {
						AddTypeParameterDefininitionsTo(type, astType);
					}
					return astType;
				}
			}
		}

		static void AddTypeParameterDefininitionsTo(ITypeDefOrRef type, AstType astType)
		{
			TypeDef typeDef = type.ResolveTypeDef();
			if (typeDef != null && typeDef.HasGenericParameters) {
				List<AstType> typeArguments = new List<AstType>();
				foreach (var gp in typeDef.GenericParameters) {
					typeArguments.Add(new SimpleType(gp.Name));
				}
				ApplyTypeArgumentsTo(astType, typeArguments);
			}
		}
		
		static void ApplyTypeArgumentsTo(AstType baseType, List<AstType> typeArguments)
		{
			SimpleType st = baseType as SimpleType;
			if (st != null) {
				st.TypeArguments.AddRange(typeArguments);
			}
			MemberType mt = baseType as MemberType;
			if (mt != null) {
				ITypeDefOrRef type = mt.Annotation<ITypeDefOrRef>();
				if (type != null) {
					int typeParameterCount;
					ICSharpCode.NRefactory.TypeSystem.ReflectionHelper.SplitTypeParameterCountFromReflectionName(type.Name, out typeParameterCount);
					if (typeParameterCount > typeArguments.Count)
						typeParameterCount = typeArguments.Count;
					mt.TypeArguments.AddRange(typeArguments.GetRange(typeArguments.Count - typeParameterCount, typeParameterCount));
					typeArguments.RemoveRange(typeArguments.Count - typeParameterCount, typeParameterCount);
					if (typeArguments.Count > 0)
						ApplyTypeArgumentsTo(mt.Target, typeArguments);
				} else {
					mt.TypeArguments.AddRange(typeArguments);
				}
			}
		}
		
		const string DynamicAttributeFullName = "System.Runtime.CompilerServices.DynamicAttribute";
		
		static bool HasDynamicAttribute(IHasCustomAttribute attributeProvider, int typeIndex)
		{
			if (attributeProvider == null || !attributeProvider.HasCustomAttributes)
				return false;
			foreach (CustomAttribute a in attributeProvider.CustomAttributes) {
				if (a.Constructor.DeclaringType.FullName == DynamicAttributeFullName) {
					if (a.ConstructorArguments.Count == 1) {
						var values = a.ConstructorArguments[0].Value as CAArgument[];
						if (values != null && typeIndex < values.Length && values[typeIndex].Value is bool)
							return (bool)values[typeIndex].Value;
					}
					return true;
				}
			}
			return false;
		}
		#endregion
		
		#region ConvertModifiers
		Modifiers ConvertModifiers(TypeDef typeDef)
		{
			Modifiers modifiers = Modifiers.None;
			if (typeDef.IsNestedPrivate)
				modifiers |= Modifiers.Private;
			else if (typeDef.IsNestedAssembly || typeDef.IsNestedFamilyAndAssembly || typeDef.IsNotPublic)
				modifiers |= Modifiers.Internal;
			else if (typeDef.IsNestedFamily)
				modifiers |= Modifiers.Protected;
			else if (typeDef.IsNestedFamilyOrAssembly)
				modifiers |= Modifiers.Protected | Modifiers.Internal;
			else if (typeDef.IsPublic || typeDef.IsNestedPublic)
				modifiers |= Modifiers.Public;
			
			if (typeDef.IsAbstract && typeDef.IsSealed)
				modifiers |= Modifiers.Static;
			else if (typeDef.IsAbstract)
				modifiers |= Modifiers.Abstract;
			else if (typeDef.IsSealed)
				modifiers |= Modifiers.Sealed;
			
			return modifiers;
		}
		
		Modifiers ConvertModifiers(FieldDef fieldDef)
		{
			Modifiers modifiers = Modifiers.None;
			if (fieldDef.IsPrivate)
				modifiers |= Modifiers.Private;
			else if (fieldDef.IsAssembly || fieldDef.IsFamilyAndAssembly)
				modifiers |= Modifiers.Internal;
			else if (fieldDef.IsFamily)
				modifiers |= Modifiers.Protected;
			else if (fieldDef.IsFamilyOrAssembly)
				modifiers |= Modifiers.Protected | Modifiers.Internal;
			else if (fieldDef.IsPublic)
				modifiers |= Modifiers.Public;
			
			if (fieldDef.IsLiteral) {
				modifiers |= Modifiers.Const;
			} else {
				if (fieldDef.IsStatic)
					modifiers |= Modifiers.Static;
				
				if (fieldDef.IsInitOnly)
					modifiers |= Modifiers.Readonly;
			}
			
			CModReqdSig modreq = fieldDef.FieldType as CModReqdSig;
			if (modreq != null && modreq.Modifier.FullName == typeof(IsVolatile).FullName)
				modifiers |= Modifiers.Volatile;
			
			return modifiers;
		}
		
		Modifiers ConvertModifiers(MethodDef methodDef)
		{
			if (methodDef == null)
				return Modifiers.None;
			Modifiers modifiers = Modifiers.None;
			if (methodDef.IsPrivate)
				modifiers |= Modifiers.Private;
			else if (methodDef.IsAssembly || methodDef.IsFamilyAndAssembly)
				modifiers |= Modifiers.Internal;
			else if (methodDef.IsFamily)
				modifiers |= Modifiers.Protected;
			else if (methodDef.IsFamilyOrAssembly)
				modifiers |= Modifiers.Protected | Modifiers.Internal;
			else if (methodDef.IsPublic)
				modifiers |= Modifiers.Public;
			
			if (methodDef.IsStatic)
				modifiers |= Modifiers.Static;
			
			if (methodDef.IsAbstract) {
				modifiers |= Modifiers.Abstract;
				if (!methodDef.IsNewSlot)
					modifiers |= Modifiers.Override;
			} else if (methodDef.IsFinal) {
				if (!methodDef.IsNewSlot) {
					modifiers |= Modifiers.Sealed | Modifiers.Override;
				}
			} else if (methodDef.IsVirtual) {
				if (methodDef.IsNewSlot)
					modifiers |= Modifiers.Virtual;
				else
					modifiers |= Modifiers.Override;
			}
			if (!methodDef.HasBody && !methodDef.IsAbstract)
				modifiers |= Modifiers.Extern;
			
			return modifiers;
		}

		#endregion
		
		void AddTypeMembers(TypeDeclaration astType, TypeDef typeDef)
		{
			// Nested types
			foreach (var nestedTypeDef in typeDef.NestedTypes) {
				if (MemberIsHidden(nestedTypeDef, context.Settings))
					continue;
				var nestedType = CreateType(nestedTypeDef);
				SetNewModifier(nestedType);
				astType.AddChild(nestedType, Roles.TypeMemberRole);
			}
			
			// Add fields
			foreach(var fieldDef in typeDef.Fields) {
				if (MemberIsHidden(fieldDef, context.Settings)) continue;
				astType.AddChild(CreateField(fieldDef), Roles.TypeMemberRole);
			}
			
			// Add events
			foreach(var eventDef in typeDef.Events) {
				astType.AddChild(CreateEvent(eventDef), Roles.TypeMemberRole);
			}

			// Add properties
			foreach(var propDef in typeDef.Properties) {
				astType.Members.Add(CreateProperty(propDef));
			}
			
			// Add methods
			foreach(var methodDef in typeDef.Methods) {
				if (MemberIsHidden(methodDef, context.Settings)) continue;
				
				if (methodDef.IsConstructor)
					astType.Members.Add(CreateConstructor(methodDef));
				else
					astType.Members.Add(CreateMethod(methodDef));
			}
		}

		EntityDeclaration CreateMethod(MethodDef methodDef)
		{
			MethodDeclaration astMethod = new MethodDeclaration();
			astMethod.AddAnnotation(methodDef);
			astMethod.ReturnType = ConvertType(methodDef.ReturnType, methodDef.Parameters.ReturnParameter.ParamDef);
			astMethod.Name = CleanName(methodDef.Name);
			astMethod.TypeParameters.AddRange(MakeTypeParameters(methodDef.GenericParameters));
			astMethod.Parameters.AddRange(MakeParameters(methodDef));
			// constraints for override and explicit interface implementation methods are inherited from the base method, so they cannot be specified directly
			if (!methodDef.IsVirtual || (methodDef.IsNewSlot && !methodDef.IsPrivate)) astMethod.Constraints.AddRange(MakeConstraints(methodDef.GenericParameters));
			if (!methodDef.DeclaringType.IsInterface) {
				if (IsExplicitInterfaceImplementation(methodDef)) {
					astMethod.PrivateImplementationType = ConvertType(methodDef.Overrides.First().MethodDeclaration.DeclaringType);
				} else {
					astMethod.Modifiers = ConvertModifiers(methodDef);
					if (methodDef.IsVirtual == methodDef.IsNewSlot)
						SetNewModifier(astMethod);
				}
				astMethod.Body = CreateMethodBody(methodDef, astMethod.Parameters);
				if (context.CurrentMethodIsAsync) {
					astMethod.Modifiers |= Modifiers.Async;
					context.CurrentMethodIsAsync = false;
				}
			}
			ConvertAttributes(astMethod, methodDef);
			if (methodDef.HasCustomAttributes && astMethod.Parameters.Count > 0) {
				foreach (CustomAttribute ca in methodDef.CustomAttributes) {
					if (ca.AttributeType.Name == "ExtensionAttribute" && ca.AttributeType.Namespace == "System.Runtime.CompilerServices") {
						astMethod.Parameters.First().ParameterModifier = ParameterModifier.This;
					}
				}
			}
			
			// Convert MethodDeclaration to OperatorDeclaration if possible
			if (methodDef.IsSpecialName && !methodDef.HasGenericParameters) {
				OperatorType? opType = OperatorDeclaration.GetOperatorType(methodDef.Name);
				if (opType.HasValue) {
					OperatorDeclaration op = new OperatorDeclaration();
					op.CopyAnnotationsFrom(astMethod);
					op.ReturnType = astMethod.ReturnType.Detach();
					op.OperatorType = opType.Value;
					op.Modifiers = astMethod.Modifiers;
					astMethod.Parameters.MoveTo(op.Parameters);
					astMethod.Attributes.MoveTo(op.Attributes);
					op.Body = astMethod.Body.Detach();
					return op;
				}
			}
			return astMethod;
		}
		
		bool IsExplicitInterfaceImplementation(MethodDef methodDef)
		{
			return methodDef.HasOverrides && methodDef.IsPrivate;
		}

		IEnumerable<TypeParameterDeclaration> MakeTypeParameters(IEnumerable<GenericParam> genericParameters)
		{
			foreach (var gp in genericParameters) {
				TypeParameterDeclaration tp = new TypeParameterDeclaration();
				tp.Name = CleanName(gp.Name);
				if (gp.IsContravariant)
					tp.Variance = VarianceModifier.Contravariant;
				else if (gp.IsCovariant)
					tp.Variance = VarianceModifier.Covariant;
				ConvertCustomAttributes(tp, gp);
				yield return tp;
			}
		}
		
		IEnumerable<Constraint> MakeConstraints(IEnumerable<GenericParam> genericParameters)
		{
			foreach (var gp in genericParameters) {
				Constraint c = new Constraint();
				c.TypeParameter = new SimpleType(CleanName(gp.Name));
				// class/struct must be first
				if (gp.HasReferenceTypeConstraint)
					c.BaseTypes.Add(new PrimitiveType("class"));
				if (gp.HasNotNullableValueTypeConstraint)
					c.BaseTypes.Add(new PrimitiveType("struct"));
				
				foreach (var constraint in gp.GenericParamConstraints) {
					if (gp.HasNotNullableValueTypeConstraint && constraint.Constraint.FullName == "System.ValueType")
						continue;
					c.BaseTypes.Add(ConvertType(constraint.Constraint));
				}
				
				if (gp.HasDefaultConstructorConstraint && !gp.HasNotNullableValueTypeConstraint)
					c.BaseTypes.Add(new PrimitiveType("new")); // new() must be last
				if (c.BaseTypes.Any())
					yield return c;
			}
		}
		
		ConstructorDeclaration CreateConstructor(MethodDef methodDef)
		{
			ConstructorDeclaration astMethod = new ConstructorDeclaration();
			astMethod.AddAnnotation(methodDef);
			astMethod.Modifiers = ConvertModifiers(methodDef);
			if (methodDef.IsStatic) {
				// don't show visibility for static ctors
				astMethod.Modifiers &= ~Modifiers.VisibilityMask;
			}
			astMethod.Name = CleanName(methodDef.DeclaringType.Name);
			astMethod.Parameters.AddRange(MakeParameters(methodDef));
			astMethod.Body = CreateMethodBody(methodDef, astMethod.Parameters);
			ConvertAttributes(astMethod, methodDef);
			if (methodDef.IsStatic && methodDef.DeclaringType.IsBeforeFieldInit && !astMethod.Body.IsNull) {
				astMethod.Body.InsertChildAfter(null, new Comment(" Note: this type is marked as 'beforefieldinit'."), Roles.Comment);
			}
			return astMethod;
		}

		Modifiers FixUpVisibility(Modifiers m)
		{
			Modifiers v = m & Modifiers.VisibilityMask;
			// If any of the modifiers is public, use that
			if ((v & Modifiers.Public) == Modifiers.Public)
				return Modifiers.Public | (m & ~Modifiers.VisibilityMask);
			// If both modifiers are private, no need to fix anything
			if (v == Modifiers.Private)
				return m;
			// Otherwise, use the other modifiers (internal and/or protected)
			return m & ~Modifiers.Private;
		}

		EntityDeclaration CreateProperty(PropertyDef propDef)
		{
			PropertyDeclaration astProp = new PropertyDeclaration();
			astProp.AddAnnotation(propDef);
			var accessor = propDef.GetMethod ?? propDef.SetMethod;
			Modifiers getterModifiers = Modifiers.None;
			Modifiers setterModifiers = Modifiers.None;
			if (IsExplicitInterfaceImplementation(accessor)) {
				astProp.PrivateImplementationType = ConvertType(accessor.Overrides.First().MethodDeclaration.DeclaringType);
			} else if (!propDef.DeclaringType.IsInterface) {
				getterModifiers = ConvertModifiers(propDef.GetMethod);
				setterModifiers = ConvertModifiers(propDef.SetMethod);
				astProp.Modifiers = FixUpVisibility(getterModifiers | setterModifiers);
				try {
					if (accessor.IsVirtual && !accessor.IsNewSlot && (propDef.GetMethod == null || propDef.SetMethod == null)) {
						foreach (var basePropDef in TypesHierarchyHelpers.FindBaseProperties(propDef)) {
							if (basePropDef.GetMethod != null && basePropDef.SetMethod != null) {
								var propVisibilityModifiers = ConvertModifiers(basePropDef.GetMethod) | ConvertModifiers(basePropDef.SetMethod);
								astProp.Modifiers = FixUpVisibility((astProp.Modifiers & ~Modifiers.VisibilityMask) | (propVisibilityModifiers & Modifiers.VisibilityMask));
								break;
							} else if ((basePropDef.GetMethod ?? basePropDef.SetMethod).IsNewSlot) {
								break;
							}
						}
					}
				} catch (ReferenceResolvingException) {
					// TODO: add some kind of notification (a comment?) about possible problems with decompiled code due to unresolved references.
				}
			}
			astProp.Name = CleanName(propDef.Name);
			astProp.ReturnType = ConvertType(propDef.PropertySig.RetType, propDef);
			
			if (propDef.GetMethod != null) {
				astProp.Getter = new Accessor();
				astProp.Getter.Body = CreateMethodBody(propDef.GetMethod);
				astProp.Getter.AddAnnotation(propDef.GetMethod);
				ConvertAttributes(astProp.Getter, propDef.GetMethod);
				
				if ((getterModifiers & Modifiers.VisibilityMask) != (astProp.Modifiers & Modifiers.VisibilityMask))
					astProp.Getter.Modifiers = getterModifiers & Modifiers.VisibilityMask;
			}
			if (propDef.SetMethod != null) {
				astProp.Setter = new Accessor();
				astProp.Setter.Body = CreateMethodBody(propDef.SetMethod);
				astProp.Setter.AddAnnotation(propDef.SetMethod);
				ConvertAttributes(astProp.Setter, propDef.SetMethod);
				Parameter lastParam = propDef.SetMethod.Parameters.LastOrDefault();
				if (lastParam != null) {
					ConvertCustomAttributes(astProp.Setter, lastParam.ParamDef, "param");
					if (lastParam.HasParamDef && lastParam.ParamDef.HasMarshalType) {
						astProp.Setter.Attributes.Add(new AttributeSection(ConvertMarshalInfo(lastParam.ParamDef, propDef.Module)) { AttributeTarget = "param" });
					}
				}
				
				if ((setterModifiers & Modifiers.VisibilityMask) != (astProp.Modifiers & Modifiers.VisibilityMask))
					astProp.Setter.Modifiers = setterModifiers & Modifiers.VisibilityMask;
			}
			ConvertCustomAttributes(astProp, propDef);

			EntityDeclaration member = astProp;
			if(propDef.IsIndexer())
				member = ConvertPropertyToIndexer(astProp, propDef);
			if(!accessor.HasOverrides && !accessor.DeclaringType.IsInterface)
				if (accessor.IsVirtual == accessor.IsNewSlot)
					SetNewModifier(member);
			return member;
		}

		IndexerDeclaration ConvertPropertyToIndexer(PropertyDeclaration astProp, PropertyDef propDef)
		{
			var astIndexer = new IndexerDeclaration();
			astIndexer.CopyAnnotationsFrom(astProp);
			astProp.Attributes.MoveTo(astIndexer.Attributes);
			astIndexer.Modifiers = astProp.Modifiers;
			astIndexer.PrivateImplementationType = astProp.PrivateImplementationType.Detach();
			astIndexer.ReturnType = astProp.ReturnType.Detach();
			astIndexer.Getter = astProp.Getter.Detach();
			astIndexer.Setter = astProp.Setter.Detach();
			astIndexer.Parameters.AddRange(MakeParameters(propDef.GetParameters()));
			return astIndexer;
		}
		
		EntityDeclaration CreateEvent(EventDef eventDef)
		{
			if (eventDef.AddMethod != null && eventDef.AddMethod.IsAbstract) {
				// An abstract event cannot be custom
				EventDeclaration astEvent = new EventDeclaration();
				ConvertCustomAttributes(astEvent, eventDef);
				astEvent.AddAnnotation(eventDef);
				astEvent.Variables.Add(new VariableInitializer(CleanName(eventDef.Name)));
				astEvent.ReturnType = ConvertType(eventDef.EventType, eventDef);
				if (!eventDef.DeclaringType.IsInterface)
					astEvent.Modifiers = ConvertModifiers(eventDef.AddMethod);
				return astEvent;
			} else {
				CustomEventDeclaration astEvent = new CustomEventDeclaration();
				ConvertCustomAttributes(astEvent, eventDef);
				astEvent.AddAnnotation(eventDef);
				astEvent.Name = CleanName(eventDef.Name);
				astEvent.ReturnType = ConvertType(eventDef.EventType, eventDef);
				if (eventDef.AddMethod == null || !IsExplicitInterfaceImplementation(eventDef.AddMethod))
					astEvent.Modifiers = ConvertModifiers(eventDef.AddMethod);
				else
					astEvent.PrivateImplementationType = ConvertType(eventDef.AddMethod.Overrides.First().MethodDeclaration.DeclaringType);
				
				if (eventDef.AddMethod != null) {
					astEvent.AddAccessor = new Accessor {
						Body = CreateMethodBody(eventDef.AddMethod)
					}.WithAnnotation(eventDef.AddMethod);
					ConvertAttributes(astEvent.AddAccessor, eventDef.AddMethod);
				}
				if (eventDef.RemoveMethod != null) {
					astEvent.RemoveAccessor = new Accessor {
						Body = CreateMethodBody(eventDef.RemoveMethod)
					}.WithAnnotation(eventDef.RemoveMethod);
					ConvertAttributes(astEvent.RemoveAccessor, eventDef.RemoveMethod);
				}
				MethodDef accessor = eventDef.AddMethod ?? eventDef.RemoveMethod;
				if (accessor.IsVirtual == accessor.IsNewSlot) {
					SetNewModifier(astEvent);
				}
				return astEvent;
			}
		}
		
		public bool DecompileMethodBodies { get; set; }
		
		BlockStatement CreateMethodBody(MethodDef method, IEnumerable<ParameterDeclaration> parameters = null)
		{
			if (DecompileMethodBodies)
				return AstMethodBodyBuilder.CreateMethodBody(method, context, parameters);
			else
				return null;
		}

		FieldDeclaration CreateField(FieldDef fieldDef)
		{
			FieldDeclaration astField = new FieldDeclaration();
			astField.AddAnnotation(fieldDef);
			VariableInitializer initializer = new VariableInitializer(CleanName(fieldDef.Name));
			astField.AddChild(initializer, Roles.Variable);
			astField.ReturnType = ConvertType(fieldDef.FieldType, fieldDef);
			astField.Modifiers = ConvertModifiers(fieldDef);
			if (fieldDef.HasConstant) {
				initializer.Initializer = CreateExpressionForConstant(fieldDef.Constant.Value, fieldDef.FieldType, fieldDef.DeclaringType.IsEnum);
			}
			ConvertAttributes(astField, fieldDef);
			SetNewModifier(astField);
			return astField;
		}
		
		static Expression CreateExpressionForConstant(object constant, TypeSig type, bool isEnumMemberDeclaration = false)
		{
			if (constant == null) {
				if (type.IsValueType && !(type.Namespace == "System" && type.TypeName == "Nullable`1"))
					return new DefaultValueExpression(ConvertType(type));
				else
					return new NullReferenceExpression();
			} else {
				TypeCode c = Type.GetTypeCode(constant.GetType());
				if (c >= TypeCode.SByte && c <= TypeCode.UInt64 && !isEnumMemberDeclaration) {
					return MakePrimitive((long)CSharpPrimitiveCast.Cast(TypeCode.Int64, constant, false), type);
				} else {
					return new PrimitiveExpression(constant);
				}
			}
		}
		
		public static IEnumerable<ParameterDeclaration> MakeParameters(MethodDef method, bool isLambda = false)
		{
			var parameters = MakeParameters(method.Parameters, isLambda);
			if (method.CallingConvention == dnlib.DotNet.CallingConvention.VarArg) {
				return parameters.Concat(new[] { new ParameterDeclaration { Type = new PrimitiveType("__arglist") } });
			} else {
				return parameters;
			}
		}
		
		public static IEnumerable<ParameterDeclaration> MakeParameters(IEnumerable<Parameter> paramCol, bool isLambda = false)
		{
			foreach(Parameter param in paramCol) {
				ParameterDeclaration astParam = new ParameterDeclaration();
				astParam.AddAnnotation(param);
				if (!(isLambda && param.Type.ContainsAnonymousType()))
					astParam.Type = ConvertType(param.Type, param.ParamDef);
				astParam.Name = param.Name;

				if (!param.HasParamDef)
					continue;
				
				if (param.Type is ByRefSig) {
					astParam.ParameterModifier = (!param.ParamDef.IsIn && param.ParamDef.IsOut) ? ParameterModifier.Out : ParameterModifier.Ref;
					ComposedType ct = astParam.Type as ComposedType;
					if (ct != null && ct.PointerRank > 0)
						ct.PointerRank--;
				}
				
				if (param.ParamDef.HasCustomAttributes) {
					foreach (CustomAttribute ca in param.ParamDef.CustomAttributes) {
						if (ca.AttributeType.Name == "ParamArrayAttribute" && ca.AttributeType.Namespace == "System")
							astParam.ParameterModifier = ParameterModifier.Params;
					}
				}
				if (param.ParamDef.IsOptional) {
					astParam.DefaultExpression = CreateExpressionForConstant(param.ParamDef.Constant.Value, param.Type);
				}
				
				ConvertCustomAttributes(astParam, param.ParamDef);
				ModuleDef module = param.Method.Module;
				if (param.ParamDef.HasMarshalType) {
					astParam.Attributes.Add(new AttributeSection(ConvertMarshalInfo(param.ParamDef, module)));
				}
				if (astParam.ParameterModifier != ParameterModifier.Out) {
					if (param.ParamDef.IsIn)
						astParam.Attributes.Add(new AttributeSection(CreateNonCustomAttribute(typeof(InAttribute), module)));
					if (param.ParamDef.IsOut)
						astParam.Attributes.Add(new AttributeSection(CreateNonCustomAttribute(typeof(OutAttribute), module)));
				}
				yield return astParam;
			}
		}
		
		#region ConvertAttributes
		void ConvertAttributes(EntityDeclaration attributedNode, TypeDef typeDefinition)
		{
			ConvertCustomAttributes(attributedNode, typeDefinition);
			ConvertSecurityAttributes(attributedNode, typeDefinition);
			
			// Handle the non-custom attributes:
			#region SerializableAttribute
			if (typeDefinition.IsSerializable)
				attributedNode.Attributes.Add(new AttributeSection(CreateNonCustomAttribute(typeof(SerializableAttribute))));
			#endregion
			
			#region ComImportAttribute
			if (typeDefinition.IsImport)
				attributedNode.Attributes.Add(new AttributeSection(CreateNonCustomAttribute(typeof(ComImportAttribute))));
			#endregion
			
			#region StructLayoutAttribute
			LayoutKind layoutKind = LayoutKind.Auto;
			switch (typeDefinition.Attributes & TypeAttributes.LayoutMask) {
				case TypeAttributes.SequentialLayout:
					layoutKind = LayoutKind.Sequential;
					break;
				case TypeAttributes.ExplicitLayout:
					layoutKind = LayoutKind.Explicit;
					break;
			}
			CharSet charSet = CharSet.None;
			switch (typeDefinition.Attributes & TypeAttributes.StringFormatMask) {
				case TypeAttributes.AnsiClass:
					charSet = CharSet.Ansi;
					break;
				case TypeAttributes.AutoClass:
					charSet = CharSet.Auto;
					break;
				case TypeAttributes.UnicodeClass:
					charSet = CharSet.Unicode;
					break;
			}
			LayoutKind defaultLayoutKind = (typeDefinition.IsValueType && !typeDefinition.IsEnum) ? LayoutKind.Sequential: LayoutKind.Auto;
			if (layoutKind != defaultLayoutKind || charSet != CharSet.Ansi || typeDefinition.HasClassLayout) {
				var structLayout = CreateNonCustomAttribute(typeof(StructLayoutAttribute));
				structLayout.Arguments.Add(new IdentifierExpression("LayoutKind").Member(layoutKind.ToString()));
				if (charSet != CharSet.Ansi) {
					structLayout.AddNamedArgument("CharSet", new IdentifierExpression("CharSet").Member(charSet.ToString()));
				}
				if (typeDefinition.PackingSize != ushort.MaxValue) {
					structLayout.AddNamedArgument("Pack", new PrimitiveExpression((int)typeDefinition.PackingSize));
				}
				if (typeDefinition.ClassSize != uint.MaxValue) {
					structLayout.AddNamedArgument("Size", new PrimitiveExpression((int)typeDefinition.ClassSize));
				}
				attributedNode.Attributes.Add(new AttributeSection(structLayout));
			}
			#endregion
		}
		
		void ConvertAttributes(EntityDeclaration attributedNode, MethodDef methodDefinition)
		{
			ConvertCustomAttributes(attributedNode, methodDefinition);
			ConvertSecurityAttributes(attributedNode, methodDefinition);
			
			MethodImplAttributes implAttributes = methodDefinition.ImplAttributes & ~MethodImplAttributes.CodeTypeMask;
			
			#region DllImportAttribute
			if (methodDefinition.HasImplMap) {
				var info = methodDefinition.ImplMap;
				var dllImport = CreateNonCustomAttribute(typeof(DllImportAttribute));
				dllImport.Arguments.Add(new PrimitiveExpression(info.Module.Name));
				
				if (info.IsBestFitDisabled)
					dllImport.AddNamedArgument("BestFitMapping", new PrimitiveExpression(false));
				if (info.IsBestFitEnabled)
					dllImport.AddNamedArgument("BestFitMapping", new PrimitiveExpression(true));
				
				System.Runtime.InteropServices.CallingConvention callingConvention;
				switch (info.Attributes & PInvokeAttributes.CallConvMask) {
					case PInvokeAttributes.CallConvCdecl:
						callingConvention = System.Runtime.InteropServices.CallingConvention.Cdecl;
						break;
					case PInvokeAttributes.CallConvFastcall:
						callingConvention = System.Runtime.InteropServices.CallingConvention.FastCall;
						break;
					case PInvokeAttributes.CallConvStdCall:
						callingConvention = System.Runtime.InteropServices.CallingConvention.StdCall;
						break;
					case PInvokeAttributes.CallConvThiscall:
						callingConvention = System.Runtime.InteropServices.CallingConvention.ThisCall;
						break;
					case PInvokeAttributes.CallConvWinapi:
						callingConvention = System.Runtime.InteropServices.CallingConvention.Winapi;
						break;
					default:
						throw new NotSupportedException("unknown calling convention");
				}
				if (callingConvention != System.Runtime.InteropServices.CallingConvention.Winapi)
					dllImport.AddNamedArgument("CallingConvention", new IdentifierExpression("CallingConvention").Member(callingConvention.ToString()));
				
				CharSet charSet = CharSet.None;
				switch (info.Attributes & PInvokeAttributes.CharSetMask) {
					case PInvokeAttributes.CharSetAnsi:
						charSet = CharSet.Ansi;
						break;
					case PInvokeAttributes.CharSetAuto:
						charSet = CharSet.Auto;
						break;
					case PInvokeAttributes.CharSetUnicode:
						charSet = CharSet.Unicode;
						break;
				}
				if (charSet != CharSet.None)
					dllImport.AddNamedArgument("CharSet", new IdentifierExpression("CharSet").Member(charSet.ToString()));

				if (!string.IsNullOrEmpty(info.Name) && info.Name != methodDefinition.Name)
					dllImport.AddNamedArgument("EntryPoint", new PrimitiveExpression(info.Name));
				
				if (info.IsNoMangle)
					dllImport.AddNamedArgument("ExactSpelling", new PrimitiveExpression(true));
				
				if ((implAttributes & MethodImplAttributes.PreserveSig) == MethodImplAttributes.PreserveSig)
					implAttributes &= ~MethodImplAttributes.PreserveSig;
				else
					dllImport.AddNamedArgument("PreserveSig", new PrimitiveExpression(false));
				
				if (info.SupportsLastError)
					dllImport.AddNamedArgument("SetLastError", new PrimitiveExpression(true));
				
				if (info.IsThrowOnUnmappableCharDisabled)
					dllImport.AddNamedArgument("ThrowOnUnmappableChar", new PrimitiveExpression(false));
				if (info.IsThrowOnUnmappableCharEnabled)
					dllImport.AddNamedArgument("ThrowOnUnmappableChar", new PrimitiveExpression(true));
				
				attributedNode.Attributes.Add(new AttributeSection(dllImport));
			}
			#endregion
			
			#region PreserveSigAttribute
			if (implAttributes == MethodImplAttributes.PreserveSig) {
				attributedNode.Attributes.Add(new AttributeSection(CreateNonCustomAttribute(typeof(PreserveSigAttribute))));
				implAttributes = 0;
			}
			#endregion
			
			#region MethodImplAttribute
			if (implAttributes != 0) {
				var methodImpl = CreateNonCustomAttribute(typeof(MethodImplAttribute));
				var methodImplOptions = methodDefinition.Module.CorLibTypes.GetTypeRef(
					"System.Runtime.CompilerServices", "MethodImplOptions");
				methodImpl.Arguments.Add(MakePrimitive((long)implAttributes, methodImplOptions.ToTypeSig()));
				attributedNode.Attributes.Add(new AttributeSection(methodImpl));
			}
			#endregion
			
			ConvertAttributes(attributedNode, methodDefinition.Parameters.ReturnParameter, methodDefinition.Module);
		}
		
		void ConvertAttributes(EntityDeclaration attributedNode, Parameter retnParam, ModuleDef module)
		{
			ConvertCustomAttributes(attributedNode, retnParam.ParamDef, "return");
			if (retnParam.HasParamDef && retnParam.ParamDef.HasMarshalType) {
				var marshalInfo = ConvertMarshalInfo(retnParam.ParamDef, module);
				attributedNode.Attributes.Add(new AttributeSection(marshalInfo) { AttributeTarget = "return" });
			}
		}
		
		internal static void ConvertAttributes(EntityDeclaration attributedNode, FieldDef fieldDefinition, string attributeTarget = null)
		{
			ConvertCustomAttributes(attributedNode, fieldDefinition);
			
			#region FieldOffsetAttribute
			if (fieldDefinition.HasLayoutInfo) {
				var fieldOffset = CreateNonCustomAttribute(typeof(FieldOffsetAttribute), fieldDefinition.Module);
				fieldOffset.Arguments.Add(new PrimitiveExpression(fieldDefinition.FieldOffset));
				attributedNode.Attributes.Add(new AttributeSection(fieldOffset) { AttributeTarget = attributeTarget });
			}
			#endregion
			
			#region NonSerializedAttribute
			if (fieldDefinition.IsNotSerialized) {
				var nonSerialized = CreateNonCustomAttribute(typeof(NonSerializedAttribute), fieldDefinition.Module);
				attributedNode.Attributes.Add(new AttributeSection(nonSerialized) { AttributeTarget = attributeTarget });
			}
			#endregion
			
			if (fieldDefinition.HasMarshalType) {
				attributedNode.Attributes.Add(new AttributeSection(ConvertMarshalInfo(fieldDefinition, fieldDefinition.Module))  { AttributeTarget = attributeTarget });
			}
		}
		
		#region MarshalAsAttribute (ConvertMarshalInfo)
		static ICSharpCode.NRefactory.CSharp.Attribute ConvertMarshalInfo(IHasFieldMarshal marshalInfoProvider, ModuleDef module)
		{
			var marshalInfo = marshalInfoProvider.MarshalType;
			var attr = CreateNonCustomAttribute(typeof(MarshalAsAttribute), module);
			var unmanagedType = module.CorLibTypes.GetTypeRef("System.Runtime.InteropServices", "UnmanagedType");
			attr.Arguments.Add(MakePrimitive((int)marshalInfo.NativeType, unmanagedType.ToTypeSig()));
			
			FixedArrayMarshalType fami = marshalInfo as FixedArrayMarshalType;
			if (fami != null) {
				attr.AddNamedArgument("SizeConst", new PrimitiveExpression(fami.Size));
				if (fami.IsElementTypeValid)
					attr.AddNamedArgument("ArraySubType", MakePrimitive((int)fami.ElementType, unmanagedType.ToTypeSig()));
			}
			SafeArrayMarshalType sami = marshalInfo as SafeArrayMarshalType;
			if (sami != null && sami.VariantType != VariantType.None) {
				var varEnum = module.CorLibTypes.GetTypeRef("System.Runtime.InteropServices", "VarEnum");
				attr.AddNamedArgument("SafeArraySubType", MakePrimitive((int)sami.VariantType, varEnum.ToTypeSig()));
			}
			ArrayMarshalType ami = marshalInfo as ArrayMarshalType;
			if (ami != null) {
				if (ami.IsElementTypeValid)
					attr.AddNamedArgument("ArraySubType", MakePrimitive((int)ami.ElementType, unmanagedType.ToTypeSig()));
				if (ami.Size >= 0)
					attr.AddNamedArgument("SizeConst", new PrimitiveExpression(ami.Size));
				if (ami.Flags != 0 && ami.ParamNumber >= 0)
					attr.AddNamedArgument("SizeParamIndex", new PrimitiveExpression(ami.ParamNumber));
			}
			CustomMarshalType cmi = marshalInfo as CustomMarshalType;
			if (cmi != null) {
				attr.AddNamedArgument("MarshalType", new PrimitiveExpression(cmi.CustomMarshaler.FullName));
				if (!string.IsNullOrEmpty(cmi.Cookie))
					attr.AddNamedArgument("MarshalCookie", new PrimitiveExpression(cmi.Cookie));
			}
			FixedSysStringMarshalType fssmi = marshalInfo as FixedSysStringMarshalType;
			if (fssmi != null) {
				attr.AddNamedArgument("SizeConst", new PrimitiveExpression(fssmi.Size));
			}
			return attr;
		}
		#endregion

		ICSharpCode.NRefactory.CSharp.Attribute CreateNonCustomAttribute(Type attributeType)
		{
			return CreateNonCustomAttribute(attributeType, context.CurrentType != null ? context.CurrentType.Module : null);
		}

		static ICSharpCode.NRefactory.CSharp.Attribute CreateNonCustomAttribute(Type attributeType, ModuleDef module)
		{
			Debug.Assert(attributeType.Name.EndsWith("Attribute", StringComparison.Ordinal));
			var attr = new ICSharpCode.NRefactory.CSharp.Attribute();
			attr.Type = new SimpleType(attributeType.Name.Substring(0, attributeType.Name.Length - "Attribute".Length));
			if (module != null) {
				attr.Type.AddAnnotation(module.CorLibTypes.GetTypeRef(attributeType.Namespace, attributeType.Name));
			}
			return attr;
		}
		
		static void ConvertCustomAttributes(AstNode attributedNode, IHasCustomAttribute customAttributeProvider, string attributeTarget = null)
		{
			if (customAttributeProvider == null)
				return;

			EntityDeclaration entityDecl = attributedNode as EntityDeclaration;
			if (customAttributeProvider.HasCustomAttributes) {
				var attributes = new List<ICSharpCode.NRefactory.CSharp.Attribute>();
				foreach (var customAttribute in customAttributeProvider.CustomAttributes.OrderBy(a => a.AttributeType.FullName)) {
					if (customAttribute.AttributeType.Name == "ExtensionAttribute" && customAttribute.AttributeType.Namespace == "System.Runtime.CompilerServices") {
						// don't show the ExtensionAttribute (it's converted to the 'this' modifier)
						continue;
					}
					if (customAttribute.AttributeType.Name == "ParamArrayAttribute" && customAttribute.AttributeType.Namespace == "System") {
						// don't show the ParamArrayAttribute (it's converted to the 'params' modifier)
						continue;
					}
					// if the method is async, remove [DebuggerStepThrough] and [Async
					if (entityDecl != null && entityDecl.HasModifier(Modifiers.Async)) {
						if (customAttribute.AttributeType.Name == "DebuggerStepThroughAttribute" && customAttribute.AttributeType.Namespace == "System.Diagnostics") {
							continue;
						}
						if (customAttribute.AttributeType.Name == "AsyncStateMachineAttribute" && customAttribute.AttributeType.Namespace == "System.Runtime.CompilerServices") {
							continue;
						}
					}
					
					var attribute = new ICSharpCode.NRefactory.CSharp.Attribute();
					attribute.AddAnnotation(customAttribute);
					attribute.Type = ConvertType(customAttribute.AttributeType);
					attributes.Add(attribute);
					
					SimpleType st = attribute.Type as SimpleType;
					if (st != null && st.Identifier.EndsWith("Attribute", StringComparison.Ordinal)) {
						st.Identifier = st.Identifier.Substring(0, st.Identifier.Length - "Attribute".Length);
					}

					if(customAttribute.HasConstructorArguments) {
						foreach (var parameter in customAttribute.ConstructorArguments) {
							Expression parameterValue = ConvertArgumentValue(parameter);
							attribute.Arguments.Add(parameterValue);
						}
					}
					if (customAttribute.Properties.Any()) {
						TypeDef resolvedAttributeType = customAttribute.AttributeType.ResolveTypeDef();
						foreach (var propertyNamedArg in customAttribute.Properties) {
							var propertyReference = resolvedAttributeType != null ? resolvedAttributeType.Properties.FirstOrDefault(pr => pr.Name == propertyNamedArg.Name) : null;
							var propertyName = new IdentifierExpression(propertyNamedArg.Name).WithAnnotation(propertyReference);
							var argumentValue = ConvertArgumentValue(propertyNamedArg.Argument);
							attribute.Arguments.Add(new AssignmentExpression(propertyName, argumentValue));
						}
					}

					if (customAttribute.Fields.Any()) {
						TypeDef resolvedAttributeType = customAttribute.AttributeType.ResolveTypeDef();
						foreach (var fieldNamedArg in customAttribute.Fields) {
							var fieldReference = resolvedAttributeType != null ? resolvedAttributeType.Fields.FirstOrDefault(f => f.Name == fieldNamedArg.Name) : null;
							var fieldName = new IdentifierExpression(fieldNamedArg.Name).WithAnnotation(fieldReference);
							var argumentValue = ConvertArgumentValue(fieldNamedArg.Argument);
							attribute.Arguments.Add(new AssignmentExpression(fieldName, argumentValue));
						}
					}
				}

				if (attributeTarget == "module" || attributeTarget == "assembly") {
					// use separate section for each attribute
					foreach (var attribute in attributes) {
						var section = new AttributeSection();
						section.AttributeTarget = attributeTarget;
						section.Attributes.Add(attribute);
						attributedNode.AddChild(section, EntityDeclaration.AttributeRole);
					}
				} else if (attributes.Count > 0) {
					// use single section for all attributes
					var section = new AttributeSection();
					section.AttributeTarget = attributeTarget;
					section.Attributes.AddRange(attributes);
					attributedNode.AddChild(section, EntityDeclaration.AttributeRole);
				}
			}
		}
		
		static void ConvertSecurityAttributes(AstNode attributedNode, IHasDeclSecurity secDeclProvider, string attributeTarget = null)
		{
			if (!secDeclProvider.HasDeclSecurities)
				return;
			var attributes = new List<ICSharpCode.NRefactory.CSharp.Attribute>();
			foreach (var secDecl in secDeclProvider.DeclSecurities.OrderBy(d => d.Action)) {
				foreach (var secAttribute in secDecl.SecurityAttributes.OrderBy(a => a.AttributeType.FullName)) {
					var attribute = new ICSharpCode.NRefactory.CSharp.Attribute();
					attribute.AddAnnotation(secAttribute);
					attribute.Type = ConvertType(secAttribute.AttributeType);
					attributes.Add(attribute);
					
					SimpleType st = attribute.Type as SimpleType;
					if (st != null && st.Identifier.EndsWith("Attribute", StringComparison.Ordinal)) {
						st.Identifier = st.Identifier.Substring(0, st.Identifier.Length - "Attribute".Length);
					}
					
					var module = secAttribute.AttributeType.Module;
					var securityActionType = module.CorLibTypes.GetTypeRef("System.Security.Permissions", "SecurityAction");
					attribute.Arguments.Add(MakePrimitive((int)secDecl.Action, securityActionType.ToTypeSig()));
					
					if (secAttribute.Properties.Any()) {
						TypeDef resolvedAttributeType = secAttribute.AttributeType.ResolveTypeDef();
						foreach (var propertyNamedArg in secAttribute.Properties) {
							var propertyReference = resolvedAttributeType != null ? resolvedAttributeType.Properties.FirstOrDefault(pr => pr.Name == propertyNamedArg.Name) : null;
							var propertyName = new IdentifierExpression(propertyNamedArg.Name).WithAnnotation(propertyReference);
							var argumentValue = ConvertArgumentValue(propertyNamedArg.Argument);
							attribute.Arguments.Add(new AssignmentExpression(propertyName, argumentValue));
						}
					}

					if (secAttribute.Fields.Any()) {
						TypeDef resolvedAttributeType = secAttribute.AttributeType.ResolveTypeDef();
						foreach (var fieldNamedArg in secAttribute.Fields) {
							var fieldReference = resolvedAttributeType != null ? resolvedAttributeType.Fields.FirstOrDefault(f => f.Name == fieldNamedArg.Name) : null;
							var fieldName = new IdentifierExpression(fieldNamedArg.Name).WithAnnotation(fieldReference);
							var argumentValue = ConvertArgumentValue(fieldNamedArg.Argument);
							attribute.Arguments.Add(new AssignmentExpression(fieldName, argumentValue));
						}
					}
				}
			}
			if (attributeTarget == "module" || attributeTarget == "assembly") {
				// use separate section for each attribute
				foreach (var attribute in attributes) {
					var section = new AttributeSection();
					section.AttributeTarget = attributeTarget;
					section.Attributes.Add(attribute);
					attributedNode.AddChild(section, EntityDeclaration.AttributeRole);
				}
			} else if (attributes.Count > 0) {
				// use single section for all attributes
				var section = new AttributeSection();
				section.AttributeTarget = attributeTarget;
				section.Attributes.AddRange(attributes);
				attributedNode.AddChild(section, EntityDeclaration.AttributeRole);
			}
		}
		
		private static Expression ConvertArgumentValue(CAArgument argument)
		{
			if (argument.Value is CAArgument[]) {
				ArrayInitializerExpression arrayInit = new ArrayInitializerExpression();
				foreach (CAArgument element in (CAArgument[])argument.Value) {
					arrayInit.Elements.Add(ConvertArgumentValue(element));
				}
				ArraySigBase arrayType = argument.Type as ArraySigBase;
				return new ArrayCreateExpression {
					Type = ConvertType(arrayType != null ? arrayType.Next : argument.Type),
					AdditionalArraySpecifiers = { new ArraySpecifier() },
					Initializer = arrayInit
				};
			} else if (argument.Value is CAArgument) {
				// occurs with boxed arguments
				return ConvertArgumentValue((CAArgument)argument.Value);
			}
			var type = argument.Type.ToTypeDefOrRef().ResolveTypeDef();
			if (type != null && type.IsEnum) {
				return MakePrimitive(Convert.ToInt64(argument.Value), argument.Type);
			} else if (argument.Value is TypeSig) {
				return CreateTypeOfExpression((TypeSig)argument.Value);
			} else if (argument.Value is UTF8String) {
				return new PrimitiveExpression(((UTF8String)argument.Value).String);
			} else {
				return new PrimitiveExpression(argument.Value);
			}
		}
		#endregion

		internal static Expression MakePrimitive(long val, TypeSig type)
		{
			if (TypeAnalysis.IsBoolean(type) && val == 0)
				return new PrimitiveExpression(false);
			else if (TypeAnalysis.IsBoolean(type) && val == 1)
				return new PrimitiveExpression(true);
			else if (val == 0 && type is PtrSig)
				return new NullReferenceExpression();
			if (type != null)
			{
				TypeDef enumDefinition = type.ToTypeDefOrRef().ResolveTypeDef();
				if (enumDefinition != null && enumDefinition.IsEnum) {
					TypeCode enumBaseTypeCode = TypeCode.Int32;
					foreach (FieldDef field in enumDefinition.Fields) {
						if (field.IsStatic && object.Equals(CSharpPrimitiveCast.Cast(TypeCode.Int64, field.Constant.Value, false), val))
							return ConvertType(type).Member(field.Name).WithAnnotation(field);
						else if (!field.IsStatic)
							enumBaseTypeCode = TypeAnalysis.GetTypeCode(field.FieldType); // use primitive type of the enum
					}
					if (IsFlagsEnum(enumDefinition)) {
						long enumValue = val;
						Expression expr = null;
						long negatedEnumValue = ~val;
						// limit negatedEnumValue to the appropriate range
						switch (enumBaseTypeCode) {
							case TypeCode.Byte:
							case TypeCode.SByte:
								negatedEnumValue &= byte.MaxValue;
								break;
							case TypeCode.Int16:
							case TypeCode.UInt16:
								negatedEnumValue &= ushort.MaxValue;
								break;
							case TypeCode.Int32:
							case TypeCode.UInt32:
								negatedEnumValue &= uint.MaxValue;
								break;
						}
						Expression negatedExpr = null;
						foreach (FieldDef field in enumDefinition.Fields.Where(fld => fld.IsStatic)) {
							long fieldValue = (long)CSharpPrimitiveCast.Cast(TypeCode.Int64, field.Constant.Value, false);
							if (fieldValue == 0)
								continue;	// skip None enum value

							if ((fieldValue & enumValue) == fieldValue) {
								var fieldExpression = ConvertType(type).Member(field.Name).WithAnnotation(field);
								if (expr == null)
									expr = fieldExpression;
								else
									expr = new BinaryOperatorExpression(expr, BinaryOperatorType.BitwiseOr, fieldExpression);

								enumValue &= ~fieldValue;
							}
							if ((fieldValue & negatedEnumValue) == fieldValue) {
								var fieldExpression = ConvertType(type).Member(field.Name).WithAnnotation(field);
								if (negatedExpr == null)
									negatedExpr = fieldExpression;
								else
									negatedExpr = new BinaryOperatorExpression(negatedExpr, BinaryOperatorType.BitwiseOr, fieldExpression);

								negatedEnumValue &= ~fieldValue;
							}
						}
						if (enumValue == 0 && expr != null) {
							if (!(negatedEnumValue == 0 && negatedExpr != null && negatedExpr.Descendants.Count() < expr.Descendants.Count())) {
								return expr;
							}
						}
						if (negatedEnumValue == 0 && negatedExpr != null) {
							return new UnaryOperatorExpression(UnaryOperatorType.BitNot, negatedExpr);
						}
					}
					return new PrimitiveExpression(CSharpPrimitiveCast.Cast(enumBaseTypeCode, val, false)).CastTo(ConvertType(type));
				}
			}
			TypeCode code = TypeAnalysis.GetTypeCode(type);
			if (code == TypeCode.Object || code == TypeCode.Empty)
				code = TypeCode.Int32;
			return new PrimitiveExpression(CSharpPrimitiveCast.Cast(code, val, false));
		}

		static bool IsFlagsEnum(TypeDef type)
		{
			if (!type.HasCustomAttributes)
				return false;

			return type.CustomAttributes.Any(attr => attr.AttributeType.FullName == "System.FlagsAttribute");
		}

		/// <summary>
		/// Sets new modifier if the member hides some other member from a base type.
		/// </summary>
		/// <param name="member">The node of the member which new modifier state should be determined.</param>
		static void SetNewModifier(EntityDeclaration member)
		{
			try {
				bool addNewModifier = false;
				if (member is IndexerDeclaration) {
					var propertyDef = member.Annotation<PropertyDef>();
					var baseProperties =
						TypesHierarchyHelpers.FindBaseProperties(propertyDef);
					addNewModifier = baseProperties.Any();
				} else
					addNewModifier = HidesBaseMember(member);

				if (addNewModifier)
					member.Modifiers |= Modifiers.New;
			}
			catch (ReferenceResolvingException) {
				// TODO: add some kind of notification (a comment?) about possible problems with decompiled code due to unresolved references.
			}
		}

		private static bool HidesBaseMember(EntityDeclaration member)
		{
			var memberDefinition = member.Annotation<IMemberDef>();
			bool addNewModifier = false;
			var methodDefinition = memberDefinition as MethodDef;
			if (methodDefinition != null) {
				addNewModifier = HidesByName(memberDefinition, includeBaseMethods: false);
				if (!addNewModifier)
					addNewModifier = TypesHierarchyHelpers.FindBaseMethods(methodDefinition).Any();
			} else
				addNewModifier = HidesByName(memberDefinition, includeBaseMethods: true);
			return addNewModifier;
		}

		/// <summary>
		/// Determines whether any base class member has the same name as the given member.
		/// </summary>
		/// <param name="member">The derived type's member.</param>
		/// <param name="includeBaseMethods">true if names of methods declared in base types should also be checked.</param>
		/// <returns>true if any base member has the same name as given member, otherwise false.</returns>
		static bool HidesByName(IMemberDef member, bool includeBaseMethods)
		{
			Debug.Assert(!(member is PropertyDef) || !((PropertyDef)member).IsIndexer());

			if (member.DeclaringType.BaseType != null) {
				var baseTypeRef = member.DeclaringType.BaseType;
				while (baseTypeRef != null) {
					var baseType = baseTypeRef.ResolveOrThrow();
					if (baseType.HasProperties && AnyIsHiddenBy(baseType.Properties, member, m => !m.IsIndexer()))
						return true;
					if (baseType.HasEvents && AnyIsHiddenBy(baseType.Events, member))
						return true;
					if (baseType.HasFields && AnyIsHiddenBy(baseType.Fields, member))
						return true;
					if (includeBaseMethods && baseType.HasMethods
					    && AnyIsHiddenBy(baseType.Methods, member, m => !m.IsSpecialName))
						return true;
					if (baseType.HasNestedTypes && AnyIsHiddenBy(baseType.NestedTypes, member))
						return true;
					baseTypeRef = baseType.BaseType;
				}
			}
			return false;
		}

		static bool AnyIsHiddenBy<T>(IEnumerable<T> members, IMemberDef derived, Predicate<T> condition = null)
			where T : IMemberDef
		{
			return members.Any(m => m.Name == derived.Name
			                   && (condition == null || condition(m))
			                   && TypesHierarchyHelpers.IsVisibleFromDerived(m, derived.DeclaringType));
		}
	}
}
