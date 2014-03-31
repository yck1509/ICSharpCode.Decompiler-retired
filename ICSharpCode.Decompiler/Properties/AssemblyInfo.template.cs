#region Using directives

using System;
using System.Resources;
using System.Reflection;
using System.Runtime.InteropServices;

#endregion

[assembly: AssemblyTitle("ICSharpCode.Decompiler")]
[assembly: AssemblyDescription("IL decompiler engine")]
[assembly: AssemblyCompany("ic#code")]
[assembly: AssemblyProduct("ILSpy")]
[assembly: AssemblyCopyright("Copyright 2011 AlphaSierraPapa for the SharpDevelop Team")]
[assembly: AssemblyTrademark("")]
[assembly: AssemblyCulture("")]

// This sets the default COM visibility of types in the assembly to invisible.
// If you need to expose a type to COM, use [ComVisible(true)] on that type.
[assembly: ComVisible(false)]

[assembly: AssemblyVersion("$INSERTVERSION$")]
[assembly: AssemblyInformationalVersion("$INSERTVERSION$$INSERTBRANCHPOSTFIX$$INSERTVERSIONNAMEPOSTFIX$-$INSERTSHORTCOMMITHASH$")]
[assembly: NeutralResourcesLanguage("en-US")]

[assembly: System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Usage", "CA2243:AttributeStringLiteralsShouldParseCorrectly",
	Justification = "AssemblyInformationalVersion does not need to be a parsable version")]

internal static class RevisionClass
{
	public const string Major = "2";
	public const string Minor = "1";
	public const string Build = "0";
	public const string Revision = "$INSERTREVISION$";
	public const string VersionName = null;
	
	public const string FullVersion = Major + "." + Minor + "." + Build + ".$INSERTREVISION$$INSERTBRANCHPOSTFIX$$INSERTVERSIONNAMEPOSTFIX$";
}
