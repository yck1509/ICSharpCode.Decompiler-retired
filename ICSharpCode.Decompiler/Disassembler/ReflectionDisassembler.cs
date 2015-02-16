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
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Text;
using System.Threading;
using dnlib.DotNet;

namespace ICSharpCode.Decompiler.Disassembler
{
	/// <summary>
	/// Disassembles type and member definitions.
	/// </summary>
	public sealed class ReflectionDisassembler
	{
		ITextOutput output;
		CancellationToken cancellationToken;
		bool isInType; // whether we are currently disassembling a whole type (-> defaultCollapsed for foldings)
		MethodBodyDisassembler methodBodyDisassembler;
		IMemberDef currentMember;
		
		public ReflectionDisassembler(ITextOutput output, bool detectControlStructure, CancellationToken cancellationToken)
		{
			if (output == null)
				throw new ArgumentNullException("output");
			this.output = output;
			this.cancellationToken = cancellationToken;
			this.methodBodyDisassembler = new MethodBodyDisassembler(output, detectControlStructure, cancellationToken);
		}
		
		#region Disassemble Method
		EnumNameCollection<MethodAttributes> methodAttributeFlags = new EnumNameCollection<MethodAttributes>() {
			{ MethodAttributes.Final, "final" },
			{ MethodAttributes.HideBySig, "hidebysig" },
			{ MethodAttributes.SpecialName, "specialname" },
			{ MethodAttributes.PInvokeImpl, null }, // handled separately
			{ MethodAttributes.UnmanagedExport, "export" },
			{ MethodAttributes.RTSpecialName, "rtspecialname" },
			{ MethodAttributes.RequireSecObject, "reqsecobj" },
			{ MethodAttributes.NewSlot, "newslot" },
			{ MethodAttributes.CheckAccessOnOverride, "strict" },
			{ MethodAttributes.Abstract, "abstract" },
			{ MethodAttributes.Virtual, "virtual" },
			{ MethodAttributes.Static, "static" },
			{ MethodAttributes.HasSecurity, null }, // ?? also invisible in ILDasm
		};
		
		EnumNameCollection<MethodAttributes> methodVisibility = new EnumNameCollection<MethodAttributes>() {
			{ MethodAttributes.Private, "private" },
			{ MethodAttributes.FamANDAssem, "famandassem" },
			{ MethodAttributes.Assembly, "assembly" },
			{ MethodAttributes.Family, "family" },
			{ MethodAttributes.FamORAssem, "famorassem" },
			{ MethodAttributes.Public, "public" },
		};

		EnumNameCollection<CallingConvention> callingConvention = new EnumNameCollection<CallingConvention>() {
			{ CallingConvention.C, "unmanaged cdecl" },
			{ CallingConvention.StdCall, "unmanaged stdcall" },
			{ CallingConvention.ThisCall, "unmanaged thiscall" },
			{ CallingConvention.FastCall, "unmanaged fastcall" },
			{ CallingConvention.VarArg, "vararg" },
			{ CallingConvention.Generic, null },
		};
		
		EnumNameCollection<MethodImplAttributes> methodCodeType = new EnumNameCollection<MethodImplAttributes>() {
			{ MethodImplAttributes.IL, "cil" },
			{ MethodImplAttributes.Native, "native" },
			{ MethodImplAttributes.OPTIL, "optil" },
			{ MethodImplAttributes.Runtime, "runtime" },
		};
		
		EnumNameCollection<MethodImplAttributes> methodImpl = new EnumNameCollection<MethodImplAttributes>() {
			{ MethodImplAttributes.Synchronized, "synchronized" },
			{ MethodImplAttributes.NoInlining, "noinlining" },
			{ MethodImplAttributes.NoOptimization, "nooptimization" },
			{ MethodImplAttributes.PreserveSig, "preservesig" },
			{ MethodImplAttributes.InternalCall, "internalcall" },
			{ MethodImplAttributes.ForwardRef, "forwardref" },
		};
		
		public void DisassembleMethod(MethodDef method)
		{
			// set current member
			currentMember = method;
			
			// write method header
			output.WriteKeyword(".method ");
			DisassembleMethodInternal(method);
		}
		
		void DisassembleMethodInternal(MethodDef method)
		{
			//    .method public hidebysig  specialname
			//               instance default class [mscorlib]System.IO.TextWriter get_BaseWriter ()  cil managed
			//
			
			//emit flags
			WriteEnum(method.Attributes & MethodAttributes.MemberAccessMask, methodVisibility);
			WriteFlags(method.Attributes & ~MethodAttributes.MemberAccessMask, methodAttributeFlags);
			if(method.IsCompilerControlled) output.WriteKeyword("privatescope ");
			
			if ((method.Attributes & MethodAttributes.PInvokeImpl) == MethodAttributes.PInvokeImpl) {
				output.WriteKeyword("pinvokeimpl");
				if (method.HasImplMap) {
					var info = method.ImplMap;
					output.Write("(");
					output.WriteLiteral("\"" + NRefactory.CSharp.CSharpOutputVisitor.ConvertString(info.Module.Name) + "\"");

					if (!string.IsNullOrEmpty(info.Name) && info.Name != method.Name) {
						output.WriteKeyword(" as ");
						output.WriteLiteral("\"" + NRefactory.CSharp.CSharpOutputVisitor.ConvertString(info.Name) + "\"");
					}

					if (info.IsNoMangle)
						output.WriteKeyword(" nomangle");
					
					if (info.IsCharSetAnsi)
						output.WriteKeyword(" ansi");
					else if (info.IsCharSetAuto)
						output.WriteKeyword(" autochar");
					else if (info.IsCharSetUnicode)
						output.WriteKeyword(" unicode");
					
					if (info.SupportsLastError)
						output.WriteKeyword(" lasterr");
					
					if (info.IsCallConvCdecl)
						output.WriteKeyword(" cdecl");
					else if (info.IsCallConvFastcall)
						output.WriteKeyword(" fastcall");
					else if (info.IsCallConvStdcall)
						output.WriteKeyword(" stdcall");
					else if (info.IsCallConvThiscall)
						output.WriteKeyword(" thiscall");
					else if (info.IsCallConvWinapi)
						output.WriteKeyword(" winapi");
					
					output.Write(')');
				}
				output.Write(' ');
			}
			
			output.WriteLine();
			output.Indent();
			if (method.ExplicitThis) {
				output.WriteKeyword("instance explicit ");
			} else if (method.HasThis) {
				output.WriteKeyword("instance ");
			}
			
			//call convention
			WriteEnum(method.CallingConvention & CallingConvention.Mask, callingConvention);
			
			//return type
			method.ReturnType.WriteTo(output);
			output.Write(' ');
			var retParam = method.Parameters.ReturnParameter;
			if (retParam != null && retParam.HasParamDef && retParam.ParamDef.HasMarshalType) {
				WriteMarshalInfo(retParam.ParamDef.MarshalType);
			}
			
			if (method.IsCompilerControlled) {
				output.WriteDefinition(DisassemblerHelpers.Escape(method.Name + "$PST" + method.MDToken.Raw.ToString("X8")), method);
			} else {
				output.WriteDefinition(DisassemblerHelpers.Escape(method.Name), method);
			}
			
			WriteTypeParameters(output, method);
			
			//( params )
			output.Write(" (");
			if (method.Parameters.Count(param => param.IsNormalMethodParameter) > 0) {
				output.WriteLine();
				output.Indent();
				WriteParameters(method.Parameters);
				output.Unindent();
			}
			output.Write(") ");
			//cil managed
			WriteEnum(method.ImplAttributes & MethodImplAttributes.CodeTypeMask, methodCodeType);
			if ((method.ImplAttributes & MethodImplAttributes.ManagedMask) == MethodImplAttributes.Managed)
				output.WriteKeyword("managed ");
			else
				output.WriteKeyword("unmanaged ");
			WriteFlags(method.ImplAttributes & ~(MethodImplAttributes.CodeTypeMask | MethodImplAttributes.ManagedMask), methodImpl);
			
			output.Unindent();
			OpenBlock(defaultCollapsed: isInType);
			WriteAttributes(method.CustomAttributes);
			if (method.HasOverrides) {
				foreach (var methodOverride in method.Overrides) {
					output.WriteKeyword(".override method ");
					methodOverride.MethodDeclaration.WriteTo(output);
					output.WriteLine();
				}
			}
			foreach (var p in method.Parameters) {
				WriteParameterAttributes(p);
			}
			WriteSecurityDeclarations(method);
			
			if (method.HasBody) {
				// create IL code mappings - used in debugger
				MemberMapping methodMapping = new MemberMapping(method);
				methodBodyDisassembler.Disassemble(method, method.Body, methodMapping);
				output.AddDebuggerMemberMapping(methodMapping);
			}
			
			CloseBlock("end of method " + DisassemblerHelpers.Escape(method.DeclaringType.Name) + "::" + DisassemblerHelpers.Escape(method.Name));
		}
		
		#region Write Security Declarations
		void WriteSecurityDeclarations(IHasDeclSecurity secDeclProvider)
		{
			if (!secDeclProvider.HasDeclSecurities)
				return;
			foreach (var secdecl in secDeclProvider.DeclSecurities) {
				output.WriteKeyword(".permissionset ");
				switch (secdecl.Action) {
					case SecurityAction.Request:
						output.WriteKeyword("request");
						break;
					case SecurityAction.Demand:
						output.WriteKeyword("demand");
						break;
					case SecurityAction.Assert:
						output.WriteKeyword("assert");
						break;
					case SecurityAction.Deny:
						output.WriteKeyword("deny");
						break;
					case SecurityAction.PermitOnly:
						output.WriteKeyword("permitonly");
						break;
					case SecurityAction.LinkDemand:
						output.WriteKeyword("linkcheck");
						break;
					case SecurityAction.InheritDemand:
						output.WriteKeyword("inheritcheck");
						break;
					case SecurityAction.RequestMinimum:
						output.WriteKeyword("reqmin");
						break;
					case SecurityAction.RequestOptional:
						output.WriteKeyword("reqopt");
						break;
					case SecurityAction.RequestRefuse:
						output.WriteKeyword("reqrefuse");
						break;
					case SecurityAction.PreJitGrant:
						output.WriteKeyword("prejitgrant");
						break;
					case SecurityAction.PreJitDeny:
						output.WriteKeyword("prejitdeny");
						break;
					case SecurityAction.NonCasDemand:
						output.WriteKeyword("noncasdemand");
						break;
					case SecurityAction.NonCasLinkDemand:
						output.WriteKeyword("noncaslinkdemand");
						break;
					case SecurityAction.NonCasInheritance:
						output.WriteKeyword("noncasinheritance");
						break;
					default:
						output.Write(secdecl.Action.ToString());
						break;
				}
				output.WriteLine(" = {");
				output.Indent();
				for (int i = 0; i < secdecl.SecurityAttributes.Count; i++) {
					SecurityAttribute sa = secdecl.SecurityAttributes[i];
					if (sa.AttributeType.Scope == sa.AttributeType.Module) {
						output.WriteKeyword("class ");
						output.Write(DisassemblerHelpers.Escape(GetAssemblyQualifiedName(sa.AttributeType)));
					} else {
						sa.AttributeType.WriteTo(output, ILNameSyntax.TypeName);
					}
					output.Write(" = {");
					if (sa.HasNamedArguments) {
						output.WriteLine();
						output.Indent();
						
						foreach (var na in sa.Fields) {
							output.WriteKeyword("field ");
							WriteSecurityDeclarationArgument(na);
							output.WriteLine();
						}
						
						foreach (var na in sa.Properties) {
							output.WriteKeyword("property ");
							WriteSecurityDeclarationArgument(na);
							output.WriteLine();
						}
						
						output.Unindent();
					}
					output.Write('}');
					
					if (i + 1 < secdecl.SecurityAttributes.Count)
						output.Write(',');
					output.WriteLine();
				}
				output.Unindent();
				output.WriteLine("}");
			}
		}
		
		void WriteSecurityDeclarationArgument(CANamedArgument na)
		{
			var type = na.Argument.Type;
			if (type is TypeDefOrRefSig) {
				var typeRef = ((TypeDefOrRefSig)type).TypeDefOrRef;
				output.WriteKeyword("enum ");
				if (typeRef.Scope != typeRef.Module) {
					output.WriteKeyword("class ");
					output.Write(DisassemblerHelpers.Escape(GetAssemblyQualifiedName(typeRef)));
				} else {
					type.WriteTo(output, ILNameSyntax.TypeName);
				}
			} else {
				type.WriteTo(output);
			}
			output.Write(' ');
			output.Write(DisassemblerHelpers.Escape(na.Name));
			output.Write(" = ");
			if (na.Argument.Value is UTF8String) {
				// secdecls use special syntax for strings
				output.WriteKeyword("string");
				output.Write("(");
				output.WriteLiteral("'" + NRefactory.CSharp.CSharpOutputVisitor.ConvertString((UTF8String)na.Argument.Value).Replace("'", "\'") + "'");
				output.Write(")");
			} else {
				WriteConstant(na.Argument.Value);
			}
		}
		
		string GetAssemblyQualifiedName(ITypeDefOrRef type)
		{
			return FullNameCreator.AssemblyQualifiedName(type, null);
		}
		#endregion
		
		#region WriteMarshalInfo
		void WriteMarshalInfo(MarshalType marshalInfo)
		{
			output.WriteKeyword("marshal");
			output.Write("(");
			WriteNativeType(marshalInfo.NativeType, marshalInfo);
			output.Write(") ");
		}

		void WriteNativeType(NativeType nativeType, MarshalType marshalInfo = null)
		{
			switch (nativeType) {
				case NativeType.Boolean:
					output.WriteKeyword("bool");
					break;
				case NativeType.I1:
					output.WriteKeyword("int8");
					break;
				case NativeType.U1:
					output.WriteKeyword("unsigned int8");
					break;
				case NativeType.I2:
					output.WriteKeyword("int16");
					break;
				case NativeType.U2:
					output.WriteKeyword("unsigned int16");
					break;
				case NativeType.I4:
					output.WriteKeyword("int32");
					break;
				case NativeType.U4:
					output.WriteKeyword("unsigned int32");
					break;
				case NativeType.I8:
					output.WriteKeyword("int64");
					break;
				case NativeType.U8:
					output.WriteKeyword("unsigned int64");
					break;
				case NativeType.R4:
					output.WriteKeyword("float32");
					break;
				case NativeType.R8:
					output.WriteKeyword("float64");
					break;
				case NativeType.LPStr:
					output.WriteKeyword("lpstr");
					break;
				case NativeType.Int:
					output.WriteKeyword("int");
					break;
				case NativeType.UInt:
					output.WriteKeyword("unsigned int");
					break;
				case NativeType.Func:
					goto default; // ??
				case NativeType.Array:
					ArrayMarshalType ami = (ArrayMarshalType)marshalInfo;
					if (ami == null)
						goto default;
					if (ami.ElementType != NativeType.Max)
						WriteNativeType(ami.ElementType);
					output.Write('[');
					if (ami.Flags == 0) {
						output.WriteLiteral(ami.Size.ToString());
					} else {
						if (ami.Size >= 0)
							output.WriteLiteral(ami.Size.ToString());
						output.Write(" + ");
						output.WriteLiteral(ami.ParamNumber.ToString());
					}
					output.Write(']');
					break;
				case NativeType.Currency:
					output.WriteKeyword("currency");
					break;
				case NativeType.BStr:
					output.WriteKeyword("bstr");
					break;
				case NativeType.LPWStr:
					output.WriteKeyword("lpwstr");
					break;
				case NativeType.LPTStr:
					output.WriteKeyword("lptstr");
					break;
				case NativeType.FixedSysString:
					output.WriteKeyword("fixed sysstring");
					output.Write("[");
					output.WriteLiteral(((FixedSysStringMarshalType)marshalInfo).Size.ToString());
					output.Write("]");
					break;
				case NativeType.IUnknown:
					output.WriteKeyword("iunknown");
					break;
				case NativeType.IDispatch:
					output.WriteKeyword("idispatch");
					break;
				case NativeType.Struct:
					output.WriteKeyword("struct");
					break;
				case NativeType.IntF:
					output.WriteKeyword("interface");
					break;
				case NativeType.SafeArray:
					output.WriteKeyword("safearray ");
					SafeArrayMarshalType sami = marshalInfo as SafeArrayMarshalType;
					if (sami != null) {
						switch (sami.VariantType) {
							case VariantType.None:
								break;
							case VariantType.I2:
								output.WriteKeyword("int16");
								break;
							case VariantType.I4:
								output.WriteKeyword("int32");
								break;
							case VariantType.R4:
								output.WriteKeyword("float32");
								break;
							case VariantType.R8:
								output.WriteKeyword("float64");
								break;
							case VariantType.CY:
								output.WriteKeyword("currency");
								break;
							case VariantType.Date:
								output.WriteKeyword("date");
								break;
							case VariantType.BStr:
								output.WriteKeyword("bstr");
								break;
							case VariantType.Dispatch:
								output.WriteKeyword("idispatch");
								break;
							case VariantType.Error:
								output.WriteKeyword("error");
								break;
							case VariantType.Bool:
								output.WriteKeyword("bool");
								break;
							case VariantType.Variant:
								output.WriteKeyword("variant");
								break;
							case VariantType.Unknown:
								output.WriteKeyword("iunknown");
								break;
							case VariantType.Decimal:
								output.WriteKeyword("decimal");
								break;
							case VariantType.I1:
								output.WriteKeyword("int8");
								break;
							case VariantType.UI1:
								output.WriteKeyword("unsigned int8");
								break;
							case VariantType.UI2:
								output.WriteKeyword("unsigned int16");
								break;
							case VariantType.UI4:
								output.WriteKeyword("unsigned int32");
								break;
							case VariantType.Int:
								output.WriteKeyword("int");
								break;
							case VariantType.UInt:
								output.WriteKeyword("unsigned int");
								break;
							default:
								output.Write(sami.VariantType.ToString());
								break;
						}
					}
					break;
				case NativeType.FixedArray:
					output.WriteKeyword("fixed array");
					FixedArrayMarshalType fami = marshalInfo as FixedArrayMarshalType;
					if (fami != null) {
						output.Write("[");
						output.WriteLiteral(fami.Size.ToString());
						output.Write("] ");
						WriteNativeType(fami.ElementType);
					}
					break;
				case NativeType.ByValStr:
					output.WriteKeyword("byvalstr");
					break;
				case NativeType.ANSIBStr:
					output.WriteKeyword("ansi bstr");
					break;
				case NativeType.TBStr:
					output.WriteKeyword("tbstr");
					break;
				case NativeType.VariantBool:
					output.WriteKeyword("variant bool");
					break;
				case NativeType.ASAny:
					output.WriteKeyword("as any");
					break;
				case NativeType.LPStruct:
					output.WriteKeyword("lpstruct");
					break;
				case NativeType.CustomMarshaler:
					CustomMarshalType cmi = marshalInfo as CustomMarshalType;
					if (cmi == null || cmi.CustomMarshaler == null)
						goto default;
					output.WriteKeyword("custom");
					output.Write("(");
					output.WriteLiteral("\"" + NRefactory.CSharp.CSharpOutputVisitor.ConvertString(cmi.CustomMarshaler.FullName) + "\"");
					output.Write(", ");
					output.WriteLiteral("\"" + NRefactory.CSharp.CSharpOutputVisitor.ConvertString(cmi.Cookie) + "\"");
					if (!UTF8String.IsNullOrEmpty(cmi.Guid) || !string.IsNullOrEmpty(cmi.NativeTypeName)) {
						output.Write(", ");
						output.WriteLiteral("\"" + cmi.Guid.ToString() + "\"");
						output.Write(", ");
						output.WriteLiteral("\"" + NRefactory.CSharp.CSharpOutputVisitor.ConvertString(cmi.NativeTypeName) + "\"");
					}
					output.Write(')');
					break;
				case NativeType.Error:
					output.WriteKeyword("error");
					break;
				default:
					output.Write(nativeType.ToString());
					break;
			}
		}
		#endregion
		
		void WriteParameters(IList<Parameter> parameters)
		{
			for (int i = 0; i < parameters.Count; i++) {
				var p = parameters[i];
				if (!p.IsNormalMethodParameter)
					continue;

				if (p.HasParamDef) {
					if (p.ParamDef.IsIn) {
						output.Write("[");
						output.WriteKeyword("in");
						output.Write("] ");
					}
					if (p.ParamDef.IsOut) {
						output.Write("[");
						output.WriteKeyword("out");
						output.Write("] ");
					}
					if (p.ParamDef.IsOptional) {
						output.Write("[");
						output.WriteKeyword("opt");
						output.Write("] ");
					}
				}
				p.Type.WriteTo(output);
				output.Write(' ');
				if (p.HasParamDef && p.ParamDef.HasMarshalType) {
					WriteMarshalInfo(p.ParamDef.MarshalType);
				}
				output.WriteDefinition(DisassemblerHelpers.Escape(p.Name), p);
				if (i < parameters.Count - 1)
					output.Write(',');
				output.WriteLine();
			}
		}
		
		bool HasParameterAttributes(Parameter p)
		{
			return p.HasParamDef && (p.ParamDef.HasConstant || p.ParamDef.HasCustomAttributes);
		}
		
		void WriteParameterAttributes(Parameter p)
		{
			if (!HasParameterAttributes(p))
				return;
			output.WriteKeyword(".param");
			output.Write(" [");
			output.WriteLiteral((p.Index + 1).ToString());
			output.Write("]");
			if (p.ParamDef.HasConstant) {
				output.Write(" = ");
				WriteConstant(p.ParamDef.Constant.Value);
			}
			output.WriteLine();
			WriteAttributes(p.ParamDef.CustomAttributes);
		}
		
		void WriteConstant(object constant)
		{
			if (constant == null) {
				output.WriteKeyword("nullref");
			} else {
				string typeName = DisassemblerHelpers.PrimitiveTypeName(constant.GetType().FullName);
				if (typeName != null && typeName != "string") {
					output.Write(typeName);
					output.Write('(');
					float? cf = constant as float?;
					double? cd = constant as double?;
					if (cf.HasValue && (float.IsNaN(cf.Value) || float.IsInfinity(cf.Value))) {
						output.WriteLiteral(string.Format("0x{0:x8}", BitConverter.ToInt32(BitConverter.GetBytes(cf.Value), 0)));
					} else if (cd.HasValue && (double.IsNaN(cd.Value) || double.IsInfinity(cd.Value))) {
						output.Write(string.Format("0x{0:x16}", BitConverter.DoubleToInt64Bits(cd.Value)));
					} else {
						DisassemblerHelpers.WriteOperand(output, constant);
					}
					output.Write(')');
				} else {
					DisassemblerHelpers.WriteOperand(output, constant);
				}
			}
		}
		#endregion
		
		#region Disassemble Field
		EnumNameCollection<FieldAttributes> fieldVisibility = new EnumNameCollection<FieldAttributes>() {
			{ FieldAttributes.Private, "private" },
			{ FieldAttributes.FamANDAssem, "famandassem" },
			{ FieldAttributes.Assembly, "assembly" },
			{ FieldAttributes.Family, "family" },
			{ FieldAttributes.FamORAssem, "famorassem" },
			{ FieldAttributes.Public, "public" },
		};
		
		EnumNameCollection<FieldAttributes> fieldAttributes = new EnumNameCollection<FieldAttributes>() {
			{ FieldAttributes.Static, "static" },
			{ FieldAttributes.Literal, "literal" },
			{ FieldAttributes.InitOnly, "initonly" },
			{ FieldAttributes.SpecialName, "specialname" },
			{ FieldAttributes.RTSpecialName, "rtspecialname" },
			{ FieldAttributes.NotSerialized, "notserialized" },
		};
		
		public void DisassembleField(FieldDef field)
		{
			output.WriteKeyword(".field ");
			if (field.HasLayoutInfo) {
				output.Write("[");
				output.WriteLiteral(field.FieldOffset.ToString());
				output.Write("]");
			}
			WriteEnum(field.Attributes & FieldAttributes.FieldAccessMask, fieldVisibility);
			const FieldAttributes hasXAttributes = FieldAttributes.HasDefault | FieldAttributes.HasFieldMarshal | FieldAttributes.HasFieldRVA;
			WriteFlags(field.Attributes & ~(FieldAttributes.FieldAccessMask | hasXAttributes), fieldAttributes);
			if (field.HasMarshalType) {
				WriteMarshalInfo(field.MarshalType);
			}
			field.FieldType.WriteTo(output);
			output.Write(' ');
			output.WriteDefinition(DisassemblerHelpers.Escape(field.Name), field);
			if ((field.Attributes & FieldAttributes.HasFieldRVA) == FieldAttributes.HasFieldRVA) {
				output.WriteKeyword(" at ");
				output.Write("I_{0:x8}", (uint)field.RVA);
			}
			if (field.HasConstant) {
				output.Write(" = ");
				WriteConstant(field.Constant.Value);
			}
			output.WriteLine();
			if (field.HasCustomAttributes) {
				output.MarkFoldStart();
				WriteAttributes(field.CustomAttributes);
				output.MarkFoldEnd();
			}
		}
		#endregion
		
		#region Disassemble Property
		EnumNameCollection<PropertyAttributes> propertyAttributes = new EnumNameCollection<PropertyAttributes>() {
			{ PropertyAttributes.SpecialName, "specialname" },
			{ PropertyAttributes.RTSpecialName, "rtspecialname" },
			{ PropertyAttributes.HasDefault, "hasdefault" },
		};
		
		public void DisassembleProperty(PropertyDef property)
		{
			// set current member
			currentMember = property;
			var sig = property.PropertySig;

			output.WriteKeyword(".property ");
			WriteFlags(property.Attributes, propertyAttributes);
			if (sig.HasThis)
				output.WriteKeyword("instance ");
			sig.RetType.WriteTo(output);
			output.Write(' ');
			output.WriteDefinition(DisassemblerHelpers.Escape(property.Name), property);
			
			output.Write("(");
			var parameters = property.GetParameters().ToList();
			if (parameters.Count(param => param.IsNormalMethodParameter) > 0) {
				output.WriteLine();
				output.Indent();
				WriteParameters(parameters);
				output.Unindent();
			}
			output.Write(")");
			
			OpenBlock(false);
			WriteAttributes(property.CustomAttributes);
			WriteNestedMethod(".get", property.GetMethod);
			WriteNestedMethod(".set", property.SetMethod);
			
			foreach (var method in property.OtherMethods) {
				WriteNestedMethod(".other", method);
			}
			CloseBlock();
		}
		
		void WriteNestedMethod(string keyword, MethodDef method)
		{
			if (method == null)
				return;

			output.WriteKeyword(keyword);
			output.Write(' ');
			method.WriteTo(output);
			output.WriteLine();
		}
		#endregion
		
		#region Disassemble Event
		EnumNameCollection<EventAttributes> eventAttributes = new EnumNameCollection<EventAttributes>() {
			{ EventAttributes.SpecialName, "specialname" },
			{ EventAttributes.RTSpecialName, "rtspecialname" },
		};
		
		public void DisassembleEvent(EventDef ev)
		{
			// set current member
			currentMember = ev;

			output.WriteKeyword(".event ");
			WriteFlags(ev.Attributes, eventAttributes);
			ev.EventType.WriteTo(output, ILNameSyntax.TypeName);
			output.Write(' ');
			output.Write(DisassemblerHelpers.Escape(ev.Name), ev);
			OpenBlock(false);
			WriteAttributes(ev.CustomAttributes);
			WriteNestedMethod(".addon", ev.AddMethod);
			WriteNestedMethod(".removeon", ev.RemoveMethod);
			WriteNestedMethod(".fire", ev.InvokeMethod);
			foreach (var method in ev.OtherMethods) {
				WriteNestedMethod(".other", method);
			}
			CloseBlock();
		}
		#endregion
		
		#region Disassemble Type
		EnumNameCollection<TypeAttributes> typeVisibility = new EnumNameCollection<TypeAttributes>() {
			{ TypeAttributes.Public, "public" },
			{ TypeAttributes.NotPublic, "private" },
			{ TypeAttributes.NestedPublic, "nested public" },
			{ TypeAttributes.NestedPrivate, "nested private" },
			{ TypeAttributes.NestedAssembly, "nested assembly" },
			{ TypeAttributes.NestedFamily, "nested family" },
			{ TypeAttributes.NestedFamANDAssem, "nested famandassem" },
			{ TypeAttributes.NestedFamORAssem, "nested famorassem" },
		};
		
		EnumNameCollection<TypeAttributes> typeLayout = new EnumNameCollection<TypeAttributes>() {
			{ TypeAttributes.AutoLayout, "auto" },
			{ TypeAttributes.SequentialLayout, "sequential" },
			{ TypeAttributes.ExplicitLayout, "explicit" },
		};
		
		EnumNameCollection<TypeAttributes> typeStringFormat = new EnumNameCollection<TypeAttributes>() {
			{ TypeAttributes.AutoClass, "auto" },
			{ TypeAttributes.AnsiClass, "ansi" },
			{ TypeAttributes.UnicodeClass, "unicode" },
		};
		
		EnumNameCollection<TypeAttributes> typeAttributes = new EnumNameCollection<TypeAttributes>() {
			{ TypeAttributes.Abstract, "abstract" },
			{ TypeAttributes.Sealed, "sealed" },
			{ TypeAttributes.SpecialName, "specialname" },
			{ TypeAttributes.Import, "import" },
			{ TypeAttributes.Serializable, "serializable" },
			{ TypeAttributes.WindowsRuntime, "windowsruntime" },
			{ TypeAttributes.BeforeFieldInit, "beforefieldinit" },
			{ TypeAttributes.HasSecurity, null },
		};
		
		public void DisassembleType(TypeDef type)
		{
			// start writing IL
			output.WriteKeyword(".class ");
			
			if ((type.Attributes & TypeAttributes.ClassSemanticMask) == TypeAttributes.Interface)
				output.WriteKeyword("interface ");
			WriteEnum(type.Attributes & TypeAttributes.VisibilityMask, typeVisibility);
			WriteEnum(type.Attributes & TypeAttributes.LayoutMask, typeLayout);
			WriteEnum(type.Attributes & TypeAttributes.StringFormatMask, typeStringFormat);
			const TypeAttributes masks = TypeAttributes.ClassSemanticMask | TypeAttributes.VisibilityMask | TypeAttributes.LayoutMask | TypeAttributes.StringFormatMask;
			WriteFlags(type.Attributes & ~masks, typeAttributes);
			
			output.WriteDefinition(DisassemblerHelpers.Escape(type.DeclaringType != null ? type.Name.String : type.FullName), type);
			WriteTypeParameters(output, type);
			output.MarkFoldStart(defaultCollapsed: isInType);
			output.WriteLine();
			
			if (type.BaseType != null) {
				output.Indent();
				output.WriteKeyword("extends ");
				type.BaseType.WriteTo(output, ILNameSyntax.TypeName);
				output.WriteLine();
				output.Unindent();
			}
			if (type.HasInterfaces) {
				output.Indent();
				for (int index = 0; index < type.Interfaces.Count; index++) {
					if (index > 0)
						output.WriteLine(",");
					if (index == 0)
						output.WriteKeyword("implements ");
					else
						output.Write("           ");
					type.Interfaces[index].Interface.WriteTo(output, ILNameSyntax.TypeName);
				}
				output.WriteLine();
				output.Unindent();
			}
			
			output.WriteLine("{");
			output.Indent();
			bool oldIsInType = isInType;
			isInType = true;
			WriteAttributes(type.CustomAttributes);
			WriteSecurityDeclarations(type);
			if (type.HasClassLayout) {
				output.WriteKeyword(".pack ");
				output.WriteLiteral(type.PackingSize.ToString());
				output.WriteLine();
				output.WriteKeyword(".size ");
				output.WriteLiteral(type.ClassSize.ToString());
				output.WriteLine();
				output.WriteLine();
			}
			if (type.HasNestedTypes) {
				output.WriteLineComment("// Nested Types");
				foreach (var nestedType in type.NestedTypes) {
					cancellationToken.ThrowIfCancellationRequested();
					DisassembleType(nestedType);
					output.WriteLine();
				}
				output.WriteLine();
			}
			if (type.HasFields) {
				output.WriteLineComment("// Fields");
				foreach (var field in type.Fields) {
					cancellationToken.ThrowIfCancellationRequested();
					DisassembleField(field);
				}
				output.WriteLine();
			}
			if (type.HasMethods) {
				output.WriteLineComment("// Methods");
				foreach (var m in type.Methods) {
					cancellationToken.ThrowIfCancellationRequested();
					DisassembleMethod(m);
					output.WriteLine();
				}
			}
			if (type.HasEvents) {
				output.WriteLineComment("// Events");
				foreach (var ev in type.Events) {
					cancellationToken.ThrowIfCancellationRequested();
					DisassembleEvent(ev);
					output.WriteLine();
				}
				output.WriteLine();
			}
			if (type.HasProperties) {
				output.WriteLineComment("// Properties");
				foreach (var prop in type.Properties) {
					cancellationToken.ThrowIfCancellationRequested();
					DisassembleProperty(prop);
				}
				output.WriteLine();
			}
			CloseBlock("end of class " + (type.DeclaringType != null ? type.Name.String : type.FullName));
			isInType = oldIsInType;
		}
		
		void WriteTypeParameters(ITextOutput output, ITypeOrMethodDef p)
		{
			if (p.NumberOfGenericParameters > 0) {
				output.Write('<');
				for (int i = 0; i < p.GenericParameters.Count; i++) {
					if (i > 0)
						output.Write(", ");
					var gp = p.GenericParameters[i];
					if (gp.HasReferenceTypeConstraint) {
						output.WriteKeyword("class ");
					} else if (gp.HasNotNullableValueTypeConstraint) {
						output.WriteKeyword("valuetype ");
					}
					if (gp.HasDefaultConstructorConstraint) {
						output.WriteKeyword(".ctor ");
					}
					if (gp.HasGenericParamConstraints) {
						output.Write('(');
						for (int j = 0; j < gp.GenericParamConstraints.Count; j++) {
							if (j > 0)
								output.Write(", ");
							gp.GenericParamConstraints[j].Constraint.WriteTo(output, ILNameSyntax.TypeName);
						}
						output.Write(") ");
					}
					if (gp.IsContravariant) {
						output.Write('-');
					} else if (gp.IsCovariant) {
						output.Write('+');
					}
					output.Write(DisassemblerHelpers.Escape(gp.Name));
				}
				output.Write('>');
			}
		}
		#endregion

		#region Helper methods
		void WriteAttributes(IList<CustomAttribute> attributes)
		{
			foreach (CustomAttribute a in attributes) {
				output.WriteKeyword(".custom ");
				a.Constructor.WriteTo(output);
				byte[] blob = a.GetBlob();
				if (blob != null) {
					output.Write(" = ");
					WriteBlob(blob);
				}
				output.WriteLine();
			}
		}
		
		void WriteBlob(byte[] blob)
		{
			output.Write("(");
			output.Indent();

			for (int i = 0; i < blob.Length; i++) {
				if (i % 16 == 0 && i < blob.Length - 1) {
					output.WriteLine();
				} else {
					output.WriteLiteral(" ");
				}
				output.WriteLiteral(blob[i].ToString("x2"));
			}
			
			output.WriteLine();
			output.Unindent();
			output.Write(")");
		}
		
		void OpenBlock(bool defaultCollapsed)
		{
			output.MarkFoldStart(defaultCollapsed: defaultCollapsed);
			output.WriteLine();
			output.WriteLine("{");
			output.Indent();
		}
		
		void CloseBlock(string comment = null)
		{
			output.Unindent();
			output.Write("}");
			if (comment != null)
				output.WriteComment(" // " + comment);
			output.MarkFoldEnd();
			output.WriteLine();
		}
		
		void WriteFlags<T>(T flags, EnumNameCollection<T> flagNames) where T : struct
		{
			long val = Convert.ToInt64(flags);
			long tested = 0;
			foreach (var pair in flagNames) {
				tested |= pair.Key;
				if ((val & pair.Key) != 0 && pair.Value != null) {
					output.WriteKeyword(pair.Value);
					output.Write(' ');
				}
			}
			if ((val & ~tested) != 0)
				output.Write("flag({0:x4}) ", val & ~tested);
		}
		
		void WriteEnum<T>(T enumValue, EnumNameCollection<T> enumNames) where T : struct
		{
			long val = Convert.ToInt64(enumValue);
			foreach (var pair in enumNames) {
				if (pair.Key == val) {
					if (pair.Value != null) {
						output.WriteKeyword(pair.Value);
						output.Write(' ');
					}
					return;
				}
			}
			if (val != 0) {
				output.Write("flag({0:x4})", val);
				output.Write(' ');
			}
			
		}
		
		sealed class EnumNameCollection<T> : IEnumerable<KeyValuePair<long, string>> where T : struct
		{
			List<KeyValuePair<long, string>> names = new List<KeyValuePair<long, string>>();
			
			public void Add(T flag, string name)
			{
				this.names.Add(new KeyValuePair<long, string>(Convert.ToInt64(flag), name));
			}
			
			public IEnumerator<KeyValuePair<long, string>> GetEnumerator()
			{
				return names.GetEnumerator();
			}
			
			System.Collections.IEnumerator System.Collections.IEnumerable.GetEnumerator()
			{
				return names.GetEnumerator();
			}
		}
		#endregion
		
		public void DisassembleNamespace(string nameSpace, IEnumerable<TypeDef> types)
		{
			if (!string.IsNullOrEmpty(nameSpace)) {
				output.WriteKeyword(".namespace ");
				output.Write(DisassemblerHelpers.Escape(nameSpace));
				OpenBlock(false);
			}
			bool oldIsInType = isInType;
			isInType = true;
			foreach (TypeDef td in types) {
				cancellationToken.ThrowIfCancellationRequested();
				DisassembleType(td);
				output.WriteLine();
			}
			if (!string.IsNullOrEmpty(nameSpace)) {
				CloseBlock();
				isInType = oldIsInType;
			}
		}
		
		public void WriteAssemblyHeader(AssemblyDef asm)
		{
			output.WriteKeyword(".assembly ");
			if (asm.IsContentTypeWindowsRuntime)
				output.WriteKeyword("windowsruntime ");
			output.Write(DisassemblerHelpers.Escape(asm.Name));
			OpenBlock(false);
			WriteAttributes(asm.CustomAttributes);
			WriteSecurityDeclarations(asm);
			if (!asm.PublicKey.IsNullOrEmpty) {
				output.WriteKeyword(".publickey");
				output.Write(" = ");
				WriteBlob(asm.PublicKey.Data);
				output.WriteLine();
			}
			if (asm.HashAlgorithm != AssemblyHashAlgorithm.None) {
				output.WriteKeyword(".hash algorithm ");
				output.WriteLiteral(string.Format("0x{0:x8}", (int)asm.HashAlgorithm));
				if (asm.HashAlgorithm == AssemblyHashAlgorithm.SHA1)
					output.WriteComment(" // SHA1");
				output.WriteLine();
			}
			Version v = asm.Version;
			if (v != null) {
				output.WriteKeyword(".ver ");
				output.WriteLiteral(string.Format("{0}:{1}:{2}:{3}", v.Major, v.Minor, v.Build, v.Revision));
				output.WriteLine();
			}
			CloseBlock();
		}
		
		public void WriteAssemblyReferences(ModuleDef module)
		{
			var moduleDef = module as ModuleDefMD;
			if (moduleDef == null)
				return;

			foreach (var mref in moduleDef.GetModuleRefs()) {
				output.WriteKeyword(".module extern ");
				output.Write(DisassemblerHelpers.Escape(mref.Name));
				output.WriteLine();
			}
			foreach (var aref in moduleDef.GetAssemblyRefs()) {
				output.WriteKeyword(".assembly extern ");
				if (aref.IsContentTypeWindowsRuntime)
					output.WriteKeyword("windowsruntime ");
				output.Write(DisassemblerHelpers.Escape(aref.Name));
				OpenBlock(false);
				if (aref.PublicKeyOrToken.Token != null && !aref.PublicKeyOrToken.Token.IsNullOrEmpty) {
					output.WriteKeyword(".publickeytoken");
					output.Write(" = ");
					WriteBlob(aref.PublicKeyOrToken.Token.Data);
					output.WriteLine();
				}
				if (aref.Version != null) {
					output.WriteKeyword(".ver ");
					output.WriteLiteral(string.Format("{0}:{1}:{2}:{3}", aref.Version.Major, aref.Version.Minor, aref.Version.Build, aref.Version.Revision));
					output.WriteLine();
				}
				CloseBlock();
			}
		}
		
		public void WriteModuleHeader(ModuleDef module)
		{
			if (module.HasExportedTypes) {
				foreach (ExportedType exportedType in module.ExportedTypes) {
					output.WriteKeyword(".class extern ");
					if (exportedType.IsForwarder)
						output.WriteKeyword("forwarder ");
					output.Write(exportedType.DeclaringType != null ? exportedType.Name.String : exportedType.FullName);
					OpenBlock(false);
					if (exportedType.DeclaringType != null) {
						output.WriteKeyword(".class extern ");
						output.WriteLine(DisassemblerHelpers.Escape(exportedType.DeclaringType.FullName));
					}
					else {
						output.WriteKeyword(".assembly extern ");
						output.WriteLine(DisassemblerHelpers.Escape(exportedType.Scope.ScopeName));
					}
					CloseBlock();
				}
			}

			output.WriteKeyword(".module ");
			output.WriteLine(module.Name);
			if (module.Mvid != null)
				output.WriteLineComment("// MVID: {0}", module.Mvid.Value.ToString("B").ToUpperInvariant());
			// TODO: imagebase, file alignment, stackreserve, subsystem
			output.WriteKeyword(".corflags ");
			output.WriteLiteral(string.Format("0x{0:x}", module.Cor20HeaderFlags));
			output.Write(" ");
			output.WriteLineComment("// {0}", module.Cor20HeaderFlags.ToString());
			
			WriteAttributes(module.CustomAttributes);
		}
		
		public void WriteModuleContents(ModuleDef module)
		{
			foreach (TypeDef td in module.Types) {
				DisassembleType(td);
				output.WriteLine();
			}
		}
	}
}
