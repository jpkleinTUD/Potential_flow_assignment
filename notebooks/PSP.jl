### A Pluto.jl notebook ###
# v0.20.4

using Markdown
using InteractiveUtils

# ╔═╡ 2281a2af-2b62-4700-8a9b-96753b795eac
begin
    # Get the directory of this notebook
    notebook_dir = @__DIR__
    # Get the project root directory (parent of notebook directory)
    project_dir = joinpath(notebook_dir, "..")
    # Activate the project environment
    import Pkg
    Pkg.activate(project_dir)
end

# ╔═╡ e551669f-0f1c-4bf3-aa3d-5589e6cf63c5
push!(LOAD_PATH, joinpath(@__DIR__, "..", "src"))

# ╔═╡ adc7fdd3-b339-4573-9717-34d7d4ac0324
using NeumannKelvin, JSON, StaticArrays, LinearAlgebra, Plots, PlotlyBase

# ╔═╡ bc054f16-5c7a-4b8d-b9af-abe0e1965573
using NeumannKelvin:kelvin,wavelike,nearfield

# ╔═╡ fa570bdd-3772-4750-980d-d75cf268ffcf
md"""

# Potential flow assignment - Pioneering Spirit slot variance study

This notebook will cover the variance study of the dimensions of the Pioneering Spirit's slot using potential flow. It has been constructed by Albert Aperghis () and Jasper Klein (5343569).

Ships come in all shapes and size's. Some are small, some are large and some are absolutely ridiculously large. Enter the Pioneering Spirit. With it's 1,000,000 m^3 maximum displacement, the vessel can be considered the largest vessel in the world based on displaced water. While this vessel boasts many remarkable features, one can't help but notice the massive missing chunk from the bow of the vessel; the slot. 

![](https://raw.githubusercontent.com/jpkleinTUD/Potential_flow_assignment/5e0a23cb8579561da9db78cd721f6f718f46b1e5/Images/PS_slot.png)

While this slot provides a lot of oppertunities from an operational point of view, it also provides challanges from a wavemaking point of view. This wavemaking characteristic will be studied further in this notebook.

After this introduction, the research question will be properly formulated. Next, the metholodogy used in this variance study will be explained. After that, the model that has been obtained by implementing the methodology will be validated by comparing the model to analytical and externally developed numerical results. After the model has been validated, the results from the variance study will be provided. Finally, the conclusion will provide a summary of the results of the study and will provide an answer to the research question.

"""

# ╔═╡ 5308c0dd-3d0a-44db-9f6b-71b9e9587dfe
md"""
## Research question


This study will discuss the effect of the various geometries of the slot (i.e. slot width, slot length, PS/SB bow bilge keel, etc.) on the wave height that can be observed within the slot.

nog ff netjes lijstje maken van wat we allemaal varieren
"""

# ╔═╡ c7786892-73cf-4e23-bfe6-339feae6f4de
md"""
## Methodology
"""

# ╔═╡ aa0202ef-8dea-4c3f-a017-fe367efca375
begin 
	# From wigley notebook
	reflect(x::SVector;flip=SA[1,-1,1]) = x.*flip
	reflect(p::NamedTuple;flip=SA[1,-1,1]) = (x=reflect(p.x;flip), 
		n=reflect(p.n;flip), dA=p.dA, x₄=reflect.(p.x₄;flip))
end

# ╔═╡ 62c1471f-5803-4b61-85fd-4a3bafde8210
function psShape(ship_info::Dict{String, Any})
		length = float(ship_info["length"])
		bow_length = float(ship_info["bow"]["lenght"])
		slot_width = float(ship_info["slot_width"])
		bow_width = float(ship_info["bow"]["width"])
		bow_radius = float(ship_info["bow"]["radius_top"]) * bow_width
	# Calculate total width of the PS        
   
        # Points for bow 1 (bottom bow)
        bow1_points = []
        
        # Start point of bow 1 (connects to rectangle)
        push!(bow1_points, (0, -bow_width - slot_width/2))
        
        # Add rounded corner points
        for θ in range(-π/2, stop=π/2, length=10)
            x = bow_length - bow_radius + bow_radius * cos(θ)
            y = -bow_width - slot_width/2 + bow_radius + bow_radius * sin(θ)
            push!(bow1_points, (x, y))
        end
        
    	flip(x) = x.*[1, -1]
        bow2_points = flip.(bow1_points)
        # Combine all points to form the complete shape
        all_points = vcat(
            (-length, -bow_width - slot_width/2),# Bottom left of rectangle
			(0.0, -bow_width - slot_width/2),      # Bottom right of rectangle
            bow1_points,
            [(bow_length-bow_radius,-slot_width/2)],# Straight part of bow 1
			[(0, -slot_width/2)],
			[(0, slot_width/2)],
            [(bow_length-bow_radius,slot_width/2)],# Straight part of bow 2
            reverse(bow2_points),  # Reverse to maintain counterclockwise order,
			(0.0, bow_width + slot_width/2), # Top right of rectangle
			(-length, bow_width + slot_width/2)     # Top left of rectangle
        )
        
        # Return the shape
        return Plots.Shape(first.(all_points), last.(all_points))
	
end


# ╔═╡ e64e3e12-d797-4fd0-b2e6-c371b8aebf82
function createPanel(vertices::Array{Array{Float64, 1}, 1}, centre::Array{Float64, 1}, normal::Array{Float64, 1}, faces::Array{Int64, 1})
	if centre[3] > 0 || normal == [0.0, 0.0, 1.0] || length(faces) < 4
		return nothing
	end
	normal = SVector{3, Float64}(normal)
	centre = SVector{3, Float64}(centre)
	face_vertices = [vertices[i+1] for i in faces]
	# Calculate area of triangle 1
	v1 = face_vertices[2] .- face_vertices[1]
	v2 = face_vertices[3] .- face_vertices[1]
	
	# Calculate area of triangle 2
	v3 = face_vertices[3] .- face_vertices[1]
	v4 = face_vertices[4] .- face_vertices[1]
	
	dA = 0.5 * norm(v1×v2) + 0.5 * norm(v3×v4)


	# Generate the four Gauss points for 2×2 quadrature (standard locations)
	# Parametric coordinates for Gauss points
	ξ₁ = SA[-1/√3, 1/√3]  # x coordinates
	# Map to physical space
	x₄ = ((ξ, η) -> SVector{3, Float64}([[f[i] for f in face_vertices]⋅[(1-ξ)*(1-η)/4, (1+ξ)*(1-η)/4, (1+ξ)*(1+η)/4, (1-ξ)*(1+η)/4] for i in range(1, length=3)])).(ξ₁, ξ₁')
	return (x=centre::SVector, n=normal::SVector ,dA = dA::Float64, x₄=x₄::SMatrix{2, 2})  
end



# ╔═╡ 520977d9-d16c-4dc1-83e2-6d6bd058662c
function importMesh(filename::String)
	import_data:: Dict{String, Any} = JSON.parsefile(filename)
	print(import_data["ship_info"]["length"])
	toArray(str:: String) = parse.(Float64, split(strip(str, ['{', '}']), ","))
	vertices = [toArray(v) for v in import_data["verts"]]
	centres = [toArray(f) for f in import_data["centres"]]
	normals = [toArray(f) for f in import_data["normals"]]
	faces = [parse.(Int64, split(strip(f, ['{', '}', 'Q']), ";")) for f in import_data["faces"] if !occursin("T", f)]
	
	panels=[createPanel(vertices, centres[i], normals[i], faces[i]) for i in eachindex(faces)]
	shape = psShape(import_data["ship_info"])
	panels = [p for p in panels if p != nothing] |> Table
	return panels, shape

end

# ╔═╡ e513f638-a083-4bfb-aa3e-6450d37001b5
# ╠═╡ disabled = true
#=╠═╡

  ╠═╡ =#

# ╔═╡ 40a64ec4-cdaa-497d-9283-c3c1da062d58
function NeumannKelvin.kelvin(ξ,α;Fn,max_z=-1/50)
	ξ[3]> 0 && throw(DomainError(ξ[3],"Sources must be below z=0"))
	x,y,z = (ξ-α)/Fn^2
	z = min(z,max_z/Fn^2) # limit z!! 💔
	(nearfield(x,y,z)+wavelike(x,abs(y),z))/Fn^2
end

# ╔═╡ 98e84ada-af82-4715-b894-6a9e1153ebb8
begin
	∫contour(x,p;Fn) = kelvin(x,p.x .* SA[1,1,0];Fn)*p.n[1]*p.dA
	function ∫surface(x,p;Fn,χ=true,dz=0)
		(!χ || p.x[3]^2 > p.dA) && return ∫kelvin(x,p;Fn,dz) # no waterline
		∫kelvin(x,p;Fn,dz)+∫contour(x,p;Fn)
	end
	function ∫surface_S₂(x,p;kwargs...)  # y-symmetric potentials
	    ∫surface(x,p;kwargs...)+∫surface(x,reflect(p,flip=SA[1,-1,1]);kwargs...)
	end
end

# ╔═╡ 264da090-b49b-4203-903c-a2fe81f165aa
data_folder = joinpath(project_dir, "src", "data")

# ╔═╡ 3006e2d4-c8b9-48a3-9857-5ab15b59238e
panels, shape = importMesh(joinpath(data_folder, "PS_hull_0305_20-26_double.json"))

# ╔═╡ 8546b716-93fc-4372-81be-f72566f8ad9d


# ╔═╡ 277b39c7-d1a6-44dc-ab94-0df434f45ebc
begin
	ps = (ϕ=∫surface,Fn=0.4)# NamedTuple of keyword-arguments
	A = influence(panels;ps...)
	b = first.(panels.n)
	q = A\b # solve for densities
end;

# ╔═╡ 4a98e4c7-95f8-4549-b7fe-99d3de917afb
begin
	using Plots: heatmap
	plt1=heatmap(A,yflip=true,colorbar_title="A")
	# plt2=plot(x,b,xlabel="x/R",margin=5mm,label="b/U",xlims=(-1,1),ylims=(-1,1))
	plot(plt1,layout=(1,2),size=(700,300))
end

# ╔═╡ 9998b3e0-a799-42a5-889a-91908d1268dd
begin

plotly()
Plots.contourf(-200:5:200,-200:5:200,(x,y)->2ζ(x,y,q,panels;ps...),
	c=:balance,aspect_ratio=:equal,clims=(-0.2,0.2));
Plots.plot!(shape, c=:black,legend=nothing)
end

# ╔═╡ 9fef423f-6f85-48ab-86fa-7687af6ce184
md"""
## Validation
intro

### Comparisson to analytical methods
In order to validate the methodlogy used to compute the wave height for varying dimensions, a comparrisson can be made to the analytical description of the wave height as reflected from a wall. The expectation is that for increasing slot width's, the wave height that is observed should converge to the this analytical description. 

The analytical description for a wave reflected from a wall can be constructed by a summation of the incoming wave, and the reflected wave. This results in the analytical expression for a standing wave:

ζ(t) = ζ_i(t) + ζ_r(t) = ζ_a cos(-k*x-ω*t) +  ζ_a cos(k*x-ω*t) = 2ζ_a cos(k*x)cos(ω*t)

For a wall located at x = 0, the expression becomes:

ζ(t) = 2ζ_a cos(ω*t)

The plot below shows the wave height for a wave refelcted by a wall in infitite waterdepth, with ζ_a = 1m, ω = 1 rad/s. Furthermore the plot shows the wave heights in the slot for increasing slot widths. 
"""

# ╔═╡ 1c89e4be-6cb6-4c0b-a3b4-b48e07617470
begin
	function wave_height_ana(t, omega=1, zeta_a=1)
		zeta_ana = (2 * zeta_a * cos(omega * t))
	end
	plot(range(0,25,100),wave_height_ana,xlims=(0, 25), ylims=(-3,3),xlabel="time [s]",ylabel="Wave height [m] - Analytical", label="Analytical Wave Height", legend=true)
end

# ╔═╡ 68af513d-c457-49f8-ba7c-d6ca7c142975
md"""
From the plot you can see.....



### Comparisson to other numerical methods
(Hopelijk die van kalea)
"""

# ╔═╡ b0df71f8-b3a3-477a-b4aa-5702491840e1
md"""
## Results
"""

# ╔═╡ 4d344c68-f99a-4df5-be6f-cdf4cff29731
md"""
## Conclusion
"""

# ╔═╡ c2437329-a343-4909-af0a-55820fcce5b3
begin
	@eval Main.PlutoRunner format_output(x::Float64; context = default_iocontext) = format_output_default(round(x; digits = 3), context)
	@eval Main.PlutoRunner format_output(x::AbstractArray{Float64}; context = default_iocontext) = format_output_default(round.(x; digits = 3), context)
end;

# ╔═╡ Cell order:
# ╠═2281a2af-2b62-4700-8a9b-96753b795eac
# ╟─e551669f-0f1c-4bf3-aa3d-5589e6cf63c5
# ╟─fa570bdd-3772-4750-980d-d75cf268ffcf
# ╟─5308c0dd-3d0a-44db-9f6b-71b9e9587dfe
# ╟─c7786892-73cf-4e23-bfe6-339feae6f4de
# ╠═adc7fdd3-b339-4573-9717-34d7d4ac0324
# ╠═aa0202ef-8dea-4c3f-a017-fe367efca375
# ╠═62c1471f-5803-4b61-85fd-4a3bafde8210
# ╟─e64e3e12-d797-4fd0-b2e6-c371b8aebf82
# ╟─520977d9-d16c-4dc1-83e2-6d6bd058662c
# ╟─bc054f16-5c7a-4b8d-b9af-abe0e1965573
# ╠═e513f638-a083-4bfb-aa3e-6450d37001b5
# ╠═40a64ec4-cdaa-497d-9283-c3c1da062d58
# ╠═98e84ada-af82-4715-b894-6a9e1153ebb8
# ╠═264da090-b49b-4203-903c-a2fe81f165aa
# ╠═3006e2d4-c8b9-48a3-9857-5ab15b59238e
# ╠═8546b716-93fc-4372-81be-f72566f8ad9d
# ╠═277b39c7-d1a6-44dc-ab94-0df434f45ebc
# ╠═4a98e4c7-95f8-4549-b7fe-99d3de917afb
# ╠═9998b3e0-a799-42a5-889a-91908d1268dd
# ╟─9fef423f-6f85-48ab-86fa-7687af6ce184
# ╟─1c89e4be-6cb6-4c0b-a3b4-b48e07617470
# ╟─68af513d-c457-49f8-ba7c-d6ca7c142975
# ╟─b0df71f8-b3a3-477a-b4aa-5702491840e1
# ╟─4d344c68-f99a-4df5-be6f-cdf4cff29731
# ╟─c2437329-a343-4909-af0a-55820fcce5b3
