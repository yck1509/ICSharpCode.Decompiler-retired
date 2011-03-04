﻿// Copyright (c) AlphaSierraPapa for the SharpDevelop Team (for details please see \doc\copyright.txt)
// This code is distributed under MIT X11 license (for details please see \doc\license.txt)

using System;
using System.ComponentModel.Composition;
using System.Windows.Input;

namespace ICSharpCode.ILSpy
{
	#region Toolbar
	public interface IToolbarCommandMetadata
	{
		string Icon { get; }
		string ToolTip { get; }
		string Category { get; }
		
		double Order { get; }
	}
	
	[MetadataAttribute]
	[AttributeUsage(AttributeTargets.Class, AllowMultiple=false)]
	public class ExportToolbarCommandAttribute : ExportAttribute
	{
		public ExportToolbarCommandAttribute()
			: base(typeof(ICommand))
		{
		}
		
		public string ToolTip { get; set; }
		public string Icon { get; set; }
		public string Category { get; set; }
		public double Order { get; set; }
	}
	#endregion
}