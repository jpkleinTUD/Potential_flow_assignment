using NURBS, FileIO, PlotlyJS

patches = load(joinpath(@__DIR__, "data", "Model_ps.stp"))
plotPatches(patches, plotControlPoints=false, resolution=0.1)
