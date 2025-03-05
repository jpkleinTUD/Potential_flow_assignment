using FileIO, NURBS, PlotlyJS

patches = load(raw"src\stp_files\PS_hull_L250p73_SW87p64_D30p00_BR6p25_20250305_17-40.dat")

# Plot the patches
plotPatches(patches, plotControlPoints=false)