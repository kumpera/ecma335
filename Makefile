foo.exe: dd/a.cs dd/b.cs
	csc /debug:portable /target:exe /out:foo.exe dd/a.cs dd/b.cs 