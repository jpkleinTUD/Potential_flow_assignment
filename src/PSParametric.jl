using Plots, NeumannKelvin
# Grid convergence
function solve_sources(panels; demi=false, Fn=0.2, verbose=false)
	if demi
		ps = (ϕ=∫surface_S₂,Fn=Fn)
	else
		ps = (ϕ=∫surface,Fn=Fn)# NamedTuple of keyword-arguments
	end
	A = influence(panels;ps...)
	
	if verbose
		A_diag = [A[i, i] for i in axes(A, 1)]
		print("Min A: $(minimum(A_diag)), Mean A: $(sum(A_diag)/length(A_diag)), Max A: $(maximum(A_diag))")
	end
	
	b = first.(panels.n)
	q = A\b # solve for densities
	return q, ps, A
end;

data_dir = joinpath(@__DIR__, "..", "src", "data", "grid_convergence");
files = readdir(data_dir);
added_masses = []
for file in file
    demi = false
    if occursin("half", file)
        demi = true;
    end
    panels = read_panels(joinpath(data_dir, file));
    q, ps, A = solve_sources(panels; demi=demi, verbose=false);
    push!(added_masses; added_mass(panels; ps));
end
