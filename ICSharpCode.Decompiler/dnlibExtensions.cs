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
using System.Linq;
using dnlib.DotNet;
using dnlib.DotNet.Emit;

namespace ICSharpCode.Decompiler
{
	/// <summary>
	/// dnlib helper methods.
	/// </summary>
	public static class dnlibExtensions
	{
		/// <summary>
		/// Gets the (exclusive) end offset of this instruction.
		/// </summary>
		public static uint GetEndOffset(this Instruction inst)
		{
			if (inst == null)
				throw new ArgumentNullException("inst");
			return inst.Offset + (uint)inst.GetSize();
		}
		
		public static string OffsetToString(uint offset)
		{
			return string.Format("IL_{0:x4}", offset);
		}
		
		public static HashSet<MethodDef> GetAccessorMethods(this TypeDef type)
		{
			HashSet<MethodDef> accessorMethods = new HashSet<MethodDef>();
			foreach (var property in type.Properties) {
				accessorMethods.Add(property.GetMethod);
				accessorMethods.Add(property.SetMethod);
				if (property.HasOtherMethods) {
					foreach (var m in property.OtherMethods)
						accessorMethods.Add(m);
				}
			}
			foreach (var ev in type.Events) {
				accessorMethods.Add(ev.AddMethod);
				accessorMethods.Add(ev.RemoveMethod);
				accessorMethods.Add(ev.InvokeMethod);
				if (ev.HasOtherMethods) {
					foreach (var m in ev.OtherMethods)
						accessorMethods.Add(m);
				}
			}
			return accessorMethods;
		}
		
		public static TypeDef ResolveWithinSameModule(this ITypeDefOrRef type)
		{
			var ownerModule = type.Module;
			if (type != null && type.Scope == type.Module)
				return type.ResolveTypeDef();
			else
				return null;
		}
		
		public static FieldDef ResolveFieldWithinSameModule(this IField field)
		{
			if (field != null && field.DeclaringType.Scope == field.Module)
				return field is FieldDef ? (FieldDef)field : ((MemberRef)field).ResolveField();
			else
				return null;
		}
		
		public static MethodDef ResolveMethodWithinSameModule(this IMethod method)
		{
			if (method != null && method.DeclaringType.Scope == method.Module)
				return method is MethodDef ? (MethodDef)method : ((MemberRef)method).ResolveMethod();
			else
				return null;
		}

		public static TypeDef ResolveOrThrow(this ITypeDefOrRef type)
		{
			try {
				return type.ResolveTypeDefThrow();
			}
			catch (TypeResolveException ex) {
				throw new ReferenceResolvingException(ex.Message, ex);
			}
		}

		public static bool IsCompilerGenerated(this IHasCustomAttribute provider)
		{
			if (provider != null && provider.HasCustomAttributes) {
				foreach (CustomAttribute a in provider.CustomAttributes) {
					if (a.AttributeType.FullName == "System.Runtime.CompilerServices.CompilerGeneratedAttribute")
						return true;
				}
			}
			return false;
		}
		
		public static bool IsCompilerGeneratedOrIsInCompilerGeneratedClass(this IMemberDef member)
		{
			if (member == null)
				return false;
			if (member.IsCompilerGenerated())
				return true;
			return IsCompilerGeneratedOrIsInCompilerGeneratedClass(member.DeclaringType);
		}

		public static TypeSig GetEnumUnderlyingType(this TypeDef type)
		{
			if (!type.IsEnum)
				throw new ArgumentException("Type must be an enum", "type");

			var fields = type.Fields;

			for (int i = 0; i < fields.Count; i++)
			{
				var field = fields[i];
				if (!field.IsStatic)
					return field.FieldType;
			}

			throw new NotSupportedException();
		}
		
		public static bool IsAnonymousType(this ITypeDefOrRef type)
		{
			if (type == null)
				return false;
			if (string.IsNullOrEmpty(type.Namespace) && type.HasGeneratedName() && (type.Name.Contains("AnonType") || type.Name.Contains("AnonymousType"))) {
				var td = type.ResolveTypeDef();
				return td != null && td.IsCompilerGenerated();
			}
			return false;
		}

		public static bool HasGeneratedName(this IMemberRef member)
		{
			return member.Name.StartsWith("<", StringComparison.Ordinal);
		}
		
		public static bool ContainsAnonymousType(this TypeSig typeSig)
		{
			GenericInstSig inst = typeSig as GenericInstSig;
			if (inst != null) {
				if (IsAnonymousType(inst.GenericType.TypeDefOrRef))
					return true;
				for (int i = 0; i < inst.GenericArguments.Count; i++) {
					if (inst.GenericArguments[i].ContainsAnonymousType())
						return true;
				}
				return false;
			}

			if (typeSig.Next == null)
				return false;

			while (typeSig.Next != null)
				typeSig = typeSig.Next;
			return typeSig.ContainsAnonymousType();
		}

		public static string GetDefaultMemberName(this TypeDef type)
		{
			CustomAttribute attr;
			return type.GetDefaultMemberName(out attr);
		}

		public static string GetDefaultMemberName(this TypeDef type, out CustomAttribute defaultMemberAttribute)
		{
			if (type.HasCustomAttributes)
				foreach (CustomAttribute ca in type.CustomAttributes)
					if (ca.Constructor.DeclaringType.Name == "DefaultMemberAttribute" && ca.Constructor.DeclaringType.Namespace == "System.Reflection"
						&& ca.Constructor.FullName == @"System.Void System.Reflection.DefaultMemberAttribute::.ctor(System.String)") {
						defaultMemberAttribute = ca;
						return ca.ConstructorArguments[0].Value as string;
					}
			defaultMemberAttribute = null;
			return null;
		}

		public static bool IsIndexer(this PropertyDef property)
		{
			CustomAttribute attr;
			return property.IsIndexer(out attr);
		}

		public static bool IsIndexer(this PropertyDef property, out CustomAttribute defaultMemberAttribute)
		{
			defaultMemberAttribute = null;
			if (property.PropertySig.Params.Count > 0) {
				var accessor = property.GetMethod ?? property.SetMethod;
				PropertyDef basePropDef = property;
				if (accessor.HasOverrides) {
					// if the property is explicitly implementing an interface, look up the property in the interface:
					MethodDef baseAccessor = accessor.Overrides.First().MethodDeclaration.ResolveMethodDef();
					if (baseAccessor != null) {
						foreach (PropertyDef baseProp in baseAccessor.DeclaringType.Properties) {
							if (baseProp.GetMethod == baseAccessor || baseProp.SetMethod == baseAccessor) {
								basePropDef = baseProp;
								break;
							}
						}
					} else
						return false;
				}
				CustomAttribute attr;
				var defaultMemberName = basePropDef.DeclaringType.GetDefaultMemberName(out attr);
				if (defaultMemberName == basePropDef.Name) {
					defaultMemberAttribute = attr;
					return true;
				}
			}
			return false;
		}

		public static IEnumerable<Parameter> GetParameters(this PropertyDef property) {
			if (property.GetMethod != null) {
				for (int i = 0; i < property.GetMethod.Parameters.Count; i++)
					yield return property.GetMethod.Parameters[i];
			}
			else if (property.SetMethod != null) {
				for (int i = 0; i < property.SetMethod.Parameters.Count - 1; i++)
					yield return property.SetMethod.Parameters[i];
			}
		}

		public static ITypeDefOrRef GetDeclaringType(this ITypeDefOrRef typeDefOrRef)
		{
			if (typeDefOrRef is TypeSpec)
				throw new NotSupportedException();
			if (typeDefOrRef is TypeDef)
				return ((TypeDef)typeDefOrRef).DeclaringType;
			else
				return ((TypeRef)typeDefOrRef).DeclaringType;
		}

		public static uint GetCodeSize(this CilBody body)
		{
			if (body.Instructions.Count == 0)
				return 0;
			else
			{
				var instr = body.Instructions.Last();
				return instr.GetEndOffset();
			}
		}

		public static Instruction GetPrevious(this Instruction instr, CilBody body)
		{
			int index = body.Instructions.IndexOf(instr);
			if (index == -1 || index == 0)
				return null;
			return body.Instructions[index - 1];
		}

		public static Instruction GetNext(this Instruction instr, CilBody body)
		{
			int index = body.Instructions.IndexOf(instr);
			if (index == -1 || index + 1 >= body.Instructions.Count)
				return null;
			return body.Instructions[index + 1];
		}

		public static bool IsMethod(object obj) {
			if (obj is IMethod) {
				if (obj is MemberRef)
					return ((MemberRef)obj).IsMethodRef;
				else
					return true;
			}
			return false;
		}

		public static bool IsField(object obj) {
			if (obj is IField) {
				if (obj is MemberRef)
					return ((MemberRef)obj).IsFieldRef;
				else
					return true;
			}
			return false;
		}

		public static bool HasReturnValue(this MethodDef method) {
			return method != null && method.ReturnType != null &&
				method.ReturnType.RemovePinnedAndModifiers().ElementType != ElementType.Void;
		}

		public static bool IsCorLibType(this ITypeDefOrRef type, string @namespace, string name) {
			return type.DefinitionAssembly.IsCorLib() && type.Namespace == @namespace && type.Name == name;
		}
	}
}
