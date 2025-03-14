using NURBS, FileIO, PlotlyJS

patches = load(joinpath(@__DIR__, "data", "Model_ps.stp"))
p=plotPatches(patches, plotControlPoints=false, resolution=0.1)
savefig(p, "nurbs_import.png")  # PNG format
savefig(p, "nurbs_import.html") # Interactive HTML (best for 3D)
display(p)