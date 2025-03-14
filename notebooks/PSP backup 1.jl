### A Pluto.jl notebook ###
# v0.20.4

using Markdown
using InteractiveUtils

# ╔═╡ bc054f16-5c7a-4b8d-b9af-abe0e1965573
# ╠═╡ disabled = true
#=╠═╡
using NeumannKelvin:kelvin,wavelike,nearfield
  ╠═╡ =#

# ╔═╡ fa570bdd-3772-4750-980d-d75cf268ffcf
md"""
# Using grasshopper as a parametric design tool for potential flow around a complex hull shape
*Albert Aperghis (5032881) and Jasper Klein (5343569)*

Ships come in all shapes and size's. Some are small, some are large and some are absolutely ridiculously large. Enter the Pioneering Spirit. With it's 1,000,000 m^3 maximum displacement, the vessel can be considered the largest vessel in the world based on displaced water. While this vessel boasts many remarkable features, one can't help but notice the massive missing chunk from the bow of the vessel; the slot. 

![](https://raw.githubusercontent.com/jpkleinTUD/Potential_flow_assignment/5e0a23cb8579561da9db78cd721f6f718f46b1e5/Images/PS_slot.png)

The original aim of this project was to perform a parametric design study on the hull shape of the Pioneering Spirit (PS). In the course of the project several challenges were encountered, and as such the decision was made to limit the scope to the following research question:

**How can Grasshopper3D be used to create complex hull shapes compatible with the NeumannKelvin.jl package**

The following sub-questions were investigated:
 - What format does the hull need to have to be exported to Julia?
 - How can the hull be converted to the NeumannKelvin packages required input format?
 - Does this method generate a realistic wake pattern?
 - Can a demi-hull be used to improve processing?
 - Are the results valid?

The final goal of the study is to develop a method to improve the efficiency of the most time consuming part of potential flow simulation - the creation of the model [1]

"""

# ╔═╡ 5308c0dd-3d0a-44db-9f6b-71b9e9587dfe
## Grasshopper
"For designers who are exploring new shapes using generative algorithms, Grasshopper® is a graphical algorithm editor tightly integrated with Rhino’s 3-D modeling tools. Unlike RhinoScript, Grasshopper requires no knowledge of programming or scripting, but still allows designers to build form generators from the simple to the awe-inspiring. " [2]

Grasshopper allows the simple creation of parametric models in Rhino-3D, without programming experience. This makes it possible to visually see the model being created in real time, a useful attribute when creating complex models.

### The model

For this project, a parametric model of the PS was made. The model features 3 primary parts. The input paramters, the creation of the Brep model, and the export to Julia. 

[Image of complete model]

The first method attempted was to save the model as a STEP file, and use the NURBS.jl package to load the geometry.  

[Code block loading in the example .stp file]

As can be seen in the plot, this does not seem to work properly. Diving into the documentation of NURBS.jl reveals the issue.

[nurbs_docs.png]

As it turns out, the majority of the model is not one of these two surface types. This setback meant a new method had to be devised. 

### Meshing and Export to julia
To work around the issue of NURBS not working on this model, Grasshopper's LunchBox module was used to create a quad mesh of the model. This method meshing  had a couple of benefits. First of all, the algorithm used implemented adaptive meshing, ensuring smaller panel sizes in places with more curvature. It also allowed the user to quickly check the mesh visually for any strange or missing sections. The target number of panels could be specified and the algorithm would do its best to create that number of panels. Finally, because the vertices were exported, determining which panels are surface panels was easy to do later on.

This mesh was then deconstructed into vertices, faces and normals. The next section will explain how these were then used to construct panels. 

[image of mesh export chain]

The file format used was JSON, as this provided the flexability to store both parameters of the the model used as well as the lists of vertices, faces and normals. This was done using a python block with the following code:

```python
import datetime
import json

def get_timestamp():
    """
    Returns the current date and time formatted as YYYYMMDD_HH-MM.
    """
    return datetime.datetime.now().strftime("%m%d_%H-%M")

n_panels = len(faces)
timestamp = get_timestamp()
filename = f"PS_hull_{timestamp}_{mode}_{n_panels}p.json"  
data = {"ship_info":{"length": length,
                        "slot_width": slot_width,
                        "bow": {"width": bow_width,
                                "radius_top": bow_radius_top,
                                "radius_side": bow_radius_bottom,
                                "length": bow_length},
                        "draught": draught,
                        "bilge_radius": bilge_radius},
            "verts": vertices,
            "faces": faces,
            "normals": normals}


if activate:
    with open(directory+filename, mode="w+") as f:
        json.dump(data, f)
```

This answers the first sub-question: "What format does the hull need to have to be exported to Julia?"

# ╔═╡ c7786892-73cf-4e23-bfe6-339feae6f4de
begin
	md"""
	## Methodology
	In order to perform a variance study, the hull form of the Pioneering Spirit needs to be described parametrically. This has been acchieved by defining the hull in Rhino, using a Grasshopper script. The combined use of grasshopper and Rhino allows for rapid definition of multiple hulls based on the geometric parameters that are to be varied for the purpose of this study. These hullforms are then be exported as NURBS data, which is used as input for the potential flow solver. (This way of working has been validated as a potential flow assignment last year, REF Rajan). The figure below provides an overview of the Rhino-Grasshopper hull definition.
	
	![]
	(https://raw.githubusercontent.com/jpkleinTUD/Potential_flow_assignment/a01588220c1a78ce20eb21a81b061a234e023fbe/Images/Rhino_grasshopper.png)
	
	For the purpose of this study, the following geometric parameters will be varied (see image below for parameter clarification):
	
	 > 1. **Slot width** 
	 > 2. **Slot length** 
	 > 3. **Bow width**
	 > 4. **Horizonal bow radius**
	 > 5. **Vertical bow radius**
	 > 6. **Outer bilge keel**
	 > 7. **Inner bilge keel**

	**     Top view          Side view          Front view**
	
	![]
	(https://raw.githubusercontent.com/jpkleinTUD/Potential_flow_assignment/refs/heads/main/Images/Dimensions_2.png)

	The variance study will be performed by implementing the following steps:

	1. **Assess the importance of total vessel length** 
	While the total vessel length is not a parameter that will be varied for the actual study, this step aims to reduce the total computation time. The hypothesis is that the wave height in the slot will not vary significantly if the total vessel size is reduced from 382m. While this step is likely introduce a slight modelling error, reducing the total vessel size will reduce the total computation time.
	
	 2. **Determine the speed range and the parameter variance range for all variables** 
	Depending on the final vessel length, the speed range for the varience study is to be determined. This speed range is to be set to froude number 0 through 1 accroding to $\text{Fn} = \frac{v}{\sqrt{g * l}}$.

	Furthermore, the range over which the geometric parameters (outlined above) are to be varried. The aim for these ranges is to vary the geometric parameters 50% above and below their real world value. The reasoning for this range is that the variance of the geometric parameters should be limited, as extreme variance in values are likely to intoduce trucation errors. (i.e. a bilge radius of 0.01m will likely not provide reliable results.)

	 3. **Compute the wave height for the full speed and parameter variance range**
	This step entails the variance study itself, where the input paramters as determined above are to be entered into the solver.
	
	
	 4. **Compute the varience in wave height caused by varying the geometric parameter**
	In order to compare the impact of the different parameters, the varience in wave height caused by changing the geometric parameters is to be computed. 
	
	 5. **Rank the parameters based on descending varience in wave height**
	Based on the varience in wave height as computed above, the parameters can be ranked accoring to which parameter has the biggest influence on the wave making characteristic of the slot.

	**@Albert, laat ff weten wat je vindt van de methode die ik heb verzonnen. Ben sws nog ff van plan om aan Gabe te vragen of we een wetenschappelijke onderbouwing nodig hebben voor onze statistische methodiek. Heb ff kort gezocht in statistiek boeken, maar dat is best een studie opzich, en wellicht buiten de scope van het vak.**
	
	"""
end

# ╔═╡ ef64d57f-30e6-45dd-b598-65728d31b77b
begin
	md"""
	## Code
	Due to limitations in the NURBS.jl package, directly exporting the geometry generated by grasshopper is not possible. To solve this issue a different method had to be used. 

	The problem was solved by creating a quadrangle mesh in grasshopper and then exporting the centres, normals and vertices of every face. 

	The $x_4$ points are approximated by interpolating $\frac{1}{\sqrt{3}}$ of the way between the vertices and the centrepoint, creating a named tuple compatible with the NeumannKelvin package. 
	"""
end

# ╔═╡ aa0202ef-8dea-4c3f-a017-fe367efca375
# ╠═╡ disabled = true
#=╠═╡
begin 
	# From wigley notebook
	reflect(x::SVector;flip=SA[1,-1,1]) = x.*flip
	reflect(p::NamedTuple;flip=SA[1,-1,1]) = (x=reflect(p.x;flip), 
		n=reflect(p.n;flip), dA=p.dA, x₄=reflect.(p.x₄;flip), wl=p.wl)
end
  ╠═╡ =#

# ╔═╡ 62c1471f-5803-4b61-85fd-4a3bafde8210
# ╠═╡ disabled = true
#=╠═╡
function psShape(ship_info::Dict{String, Any});
		length = float(ship_info["length"]);
		bow_length = float(ship_info["bow"]["length"]);
		slot_width = float(ship_info["slot_width"]);
		bow_width = float(ship_info["bow"]["width"]);
		bow_radius = float(ship_info["bow"]["radius_top"]) * bow_width/2;
	# Calculate total width of the PS        
   
        # Points for bow 1 (bottom bow)
        bow1_points = [];
        
        # Start point of bow 1 (connects to rectangle)
        push!(bow1_points, (0, -bow_width - slot_width/2));
        
        # Add rounded corner points
        for θ in range(-π/2, stop=0, length=6)
            x = bow_length - bow_radius + bow_radius * cos(θ);
            y = - slot_width/2 - bow_width + bow_radius * (1 + sin(θ));
            push!(bow1_points, (x, y));
        end
		for θ in range(0, stop=π/2, length=6)
			x = bow_length - bow_radius + bow_radius * cos(θ);
			y = -slot_width/2 - bow_radius * (1-sin(θ));
			push!(bow1_points, (x, y));
		end
    	flip(x) = x.*[1, -1];
        bow2_points = flip.(bow1_points);
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
        );
        
        # Return the shape
        return Plots.Shape(first.(all_points), last.(all_points));
	
end

  ╠═╡ =#

# ╔═╡ f41c5399-987a-4122-b283-76f488eaabb7
md"""### Main functions"""

# ╔═╡ e64e3e12-d797-4fd0-b2e6-c371b8aebf82


# ╔═╡ 1d5b75bc-9b23-473d-bc43-5d492c621d5d
# ╠═╡ disabled = true
#=╠═╡
function createPanel(vertices::Array{Array{Float64, 1}, 1},
					 normals::Array{Array{Float64, 1}, 1}, 
					 faces::Array{Int64, 1});
	
	face_vertices = [vertices[i+1] for i in faces];
	face_normals = [normals[i+1] for i in faces];
	centre = sum(face_vertices)/4
	normal = sum(face_normals)
	normal /= norm(normal)

	(length(faces) < 4) && return nothing;
	
	wl_panel = false;
	
	if any([vertex[3]>-1/50 for vertex in face_vertices])
		wl_panel=true
	end
	
    # Calculating dA as two triangles
	v1 = face_vertices[2] .- face_vertices[1];
	v2 = face_vertices[3] .- face_vertices[1];
	
	v3 = face_vertices[3] .- face_vertices[1];
	v4 = face_vertices[4] .- face_vertices[1];
	
	dA = 0.5 * norm(v1×v2) + 0.5 * norm(v3×v4);

	# Generate the four Gauss points for 2×2 quadrature
	ξ₁ = SA[-1/√3, 1/√3];
	# Map to physical space
	x₄ = ((ξ, η) -> SVector{3, Float64}([[f[i] for f in face_vertices]⋅[(1-ξ)*(1-η)/4, (1+ξ)*(1-η)/4, (1+ξ)*(1+η)/4, (1-ξ)*(1+η)/4] for i in range(1, length=3)])).(ξ₁, ξ₁');
	
	normal = SVector{3, Float64}(normal);
	centre = SVector{3, Float64}(centre);
	return (x=centre::SVector, n=normal::SVector ,dA = dA::Float64, x₄=x₄::SMatrix{2, 2}, wl=wl_panel::Bool);
end


  ╠═╡ =#

# ╔═╡ 520977d9-d16c-4dc1-83e2-6d6bd058662c
# ╠═╡ disabled = true
#=╠═╡
function importMesh(filename::String);
	import_data:: Dict{String, Any} = JSON.parsefile(filename);
	toArray(str:: String) = parse.(Float64, split(strip(str, ['{', '}']), ","));
	vertices = [toArray(v) for v in import_data["verts"]];
	normals = [toArray(f) for f in import_data["normals"]];
	faces = [parse.(Int64, split(strip(f, ['{', '}', 'Q']), ";")) for f in import_data["faces"] if !occursin("T", f)];

    face_count = length(faces);
    println("Number of faces: $(face_count)");
    if face_count > 10000 # Protecting ourselves from crashing (again)
        println("Too many faces ($(face_count) > 10000), skipping import");
        return nothing, nothing, 0.0, nothing;
    end

	# Creating panels from the vertices, normals and faces from Grasshopper
	panels=[createPanel(vertices, normals, face) for face in faces];
	panels = [p for p in panels if p != nothing] |> Table; # Removing ignored panels

	# Creating shape for plot overlay
	shape = psShape(import_data["ship_info"]);

	total_length = import_data["ship_info"]["length"] + import_data["ship_info"]["bow"]["length"];
	
	# Determining the mean panel lengthscale to compare models
	h_mean = √(sum(panels.dA)/length(panels))
	return panels, shape, total_length, h_mean;

end
  ╠═╡ =#

# ╔═╡ e513f638-a083-4bfb-aa3e-6450d37001b5
md"""The following functions are taken from the notebooks written in class"""


# ╔═╡ 5fa06031-f734-42e7-95bf-fd0daf506687
md"""
The surface potential has been adjusted to work with the definition of the waterline being all surfaces located at ``z=\frac{-1}{50}``
"""

# ╔═╡ 264da090-b49b-4203-903c-a2fe81f165aa
# ╠═╡ disabled = true
#=╠═╡
data_folder = joinpath(@__DIR__, "..", "src", "data");
  ╠═╡ =#

# ╔═╡ 7e195849-db71-401b-8c3c-68c712135390
md"""
#### Importing a custom mesh
"""

# ╔═╡ 3006e2d4-c8b9-48a3-9857-5ab15b59238e
# ╠═╡ disabled = true
#=╠═╡
panels, shape, length_ps, h_mean = importMesh(joinpath(data_folder, "small_ps/PS_hull_0312_21-05_double_small_fine.json"));
  ╠═╡ =#

# ╔═╡ a0c223be-1c3e-4fc2-aa5b-e6b6e077eb40
# ╠═╡ disabled = true
#=╠═╡
md"""
Plot panels? $(@bind plot_panels CheckBox(default=false))
"""
  ╠═╡ =#

# ╔═╡ b2203672-3079-49ec-a7f4-e09804136b86
# ╠═╡ disabled = true
#=╠═╡
begin
	if plot_panels
		Plots.scatter3d(
			eachrow(stack(panels.x))...,label=nothing,
			marker_z=@.(panels.wl),
			c=palette([:grey,:green], 2),
			title = "PS hull with waterline panels marked", aspect_ratio=:equal)
	end
end
  ╠═╡ =#

# ╔═╡ 8546b716-93fc-4372-81be-f72566f8ad9d
md"""Using the code from class the source strengths can be solved"""

# ╔═╡ f34a6fa6-2d59-41ad-93fd-e431c52357c9
# ╠═╡ disabled = true
#=╠═╡
md"""
Then the source strengths can be calculated for any given Froude Number (Lenght based)

**``Fn_l`` =  $(@bind Fn1 confirm(Slider(0.01:0.01:0.4, default=0.2, show_value=true)))**

The corresponding velocity is $(round(1.944 * Fn1√(9.81*length_ps); digits=2))kts
"""
  ╠═╡ =#

# ╔═╡ 301d6aed-a9f6-4a3c-9a41-0a4f693e1355
# ╠═╡ disabled = true
#=╠═╡
begin
	md"""
	#### Free surface elevation of the model at Fn=$(Fn1)

	plot? $(@bind plot_contour_1 CheckBox(default=false))
	"""
end
  ╠═╡ =#

# ╔═╡ 9fef423f-6f85-48ab-86fa-7687af6ce184
md"""
## Verification
intro

### Comparisson to wigley hull
In order to validate the output from the model create using the grasshopper script, a reference that resembles the geometry of the grasshopper model had to be found. This reference was constructed by adapting the wigley hull as defined notebook "wigley.jl" (REF TO NOTEBOOK). 

The first step in the process of constructing a resembling geometry was to double the wigley hull, with an offset with respect to the center line. For this, it was chosen to explictly define the complete hull, instead of mirroring a half hull. While the mirroring would reduce computational time, the process of mirroring a half hull has the potential to create an additional source of human error. Since this geometry will be used as verification of the grasshopper model, minimizing the potential for error has been considered a priority above computational time. 

The code block below shows the definition of two wigley hull's, that have an offset with respect to the center line. Below the code, the panels as created by the code block and the potential flow solution are plotted.
"""

# ╔═╡ 110b514a-6666-48f6-ba52-4b188caf9ca3
# ╠═╡ disabled = true
#=╠═╡
begin
	B = 0.2
	offset = 5/2
	L = 1

	# drawing the double hull in a plot
	wigley_WL(x, offset, unit) = unit*0.5B*(1-(2x)^2)+(B*offset)/2 
	wigley_shape_l_1(h,x=-(L/2):h:(L/2)) = Plots.Shape(x,wigley_WL.(x, offset, 1))
	wigley_shape_r_1(h,x=-(L/2):h:(L/2)) = Plots.Shape(x,wigley_WL.(x, offset, -1))
	wigley_shape_l_2(h,x=-(L/2):h:(L/2)) = Plots.Shape(x,wigley_WL.(x,-offset, 1))
	wigley_shape_r_2(h,x=-(L/2):h:(L/2)) = Plots.Shape(x,wigley_WL.(x,-offset, -1))

	function wigley_hull(hx,hz;D=1/8)
		# parabolic width equation and scaled 3D surface for the PS part of PS hull
		η_l_1(ξ,ζ) = (1-ξ^2)*(1-ζ^2)+offset                
	    S_l_1(ξ,ζ) = SA[0.5L*ξ,0.5B*η_l_1(ξ,ζ),-D*ζ]

		# parabolic width equation and scaled 3D surface for the SB part of PS hull
		η_r_1(ξ,ζ) = -((1-ξ^2)*(1-ζ^2)-offset)                
	    S_r_1(ξ,ζ) = SA[0.5L*ξ,0.5B*η_r_1(ξ,ζ),-D*ζ]    

		# parabolic width equation and scaled 3D surface for the PS part of SB hull
		η_l_2(ξ,ζ) = (1-ξ^2)*(1-ζ^2)-offset
	    S_l_2(ξ,ζ) = SA[0.5L*ξ,0.5B*η_l_2(ξ,ζ),-D*ζ]

		# parabolic width equation and scaled 3D surface for the SB part of SB hull
		η_r_2(ξ,ζ) = -((1-ξ^2)*(1-ζ^2)+offset)               
	    S_r_2(ξ,ζ) = SA[0.5L*ξ,0.5B*η_r_2(ξ,ζ),-D*ζ]
		
	    dξ = 1/round(0.5L/hx); ξ = 0.5dξ-1:dξ:1 # sampling in ξ
	    dζ = 1/round(D/hz); ζ = 0.5dζ:dζ:1      # sampling in ζ
		
	    # explicit defintion of all hull parts
		panels_l_1 = param_props.(S_l_1,ξ,ζ',dξ,dζ) |> Table     
	    panels_r_1 = param_props.(S_r_1,ξ,ζ',-dξ,dζ) |> Table
		panels_l_2 = param_props.(S_l_2,ξ,ζ',dξ,dζ) |> Table
		panels_r_2 = param_props.(S_r_2,ξ,ζ',-dξ,dζ) |> Table 

		# return concatinated hull
		return vcat(panels_l_1, panels_r_1, panels_l_2, panels_r_2)
	end
end
  ╠═╡ =#

# ╔═╡ 3079d163-b5b0-4ad8-aaeb-0c32fe721f21
#=╠═╡
begin
	h = 1/32
	doublehull = wigley_hull(h,h); length(doublehull) 
	Plots.scatter3d(
	eachrow(stack(doublehull.x))...,label=nothing,
	ylims=(-1,1),zlims=(-0.5,0.5),
	marker_z=@.(last(doublehull.x)^2<doublehull.dA),
	c=palette([:grey,:green], 2),
	title = "Double Wigley hull with waterline panels marked")
end
  ╠═╡ =#

# ╔═╡ 277b39c7-d1a6-44dc-ab94-0df434f45ebc
# ╠═╡ disabled = true
#=╠═╡
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
  ╠═╡ =#

# ╔═╡ 9998b3e0-a799-42a5-889a-91908d1268dd
# ╠═╡ disabled = true
#=╠═╡
begin
if plot_contour_1
    plotly()
    Plots.contourf(-2:h_mean:2,-2:h_mean:2, (x,y)->2ζ(x,y,q,panels;ps...),
        c=:balance, aspect_ratio=:equal);
    Plots.plot!(shape, c=:blue,legend=nothing)
end
end
  ╠═╡ =#

# ╔═╡ 744044c4-0ca8-400c-b049-71e16ef052d9
# ╠═╡ disabled = true
#=╠═╡
added_mass(panels; ps)
  ╠═╡ =#

# ╔═╡ 1c89e4be-6cb6-4c0b-a3b4-b48e07617470
# ╠═╡ disabled = true
#=╠═╡
Plots.contourf(-1.5:h:1,-1:h:1,(x,y)->ζ(x,y,q,doublehull;ps...),
	c=:balance,aspect_ratio=:equal,clims=(-0.3,0.3));Plots.plot!(
	wigley_shape_l_1(h),c=:black,legend=nothing);Plots.plot!(
	wigley_shape_r_1(h),c=:black,legend=nothing);Plots.plot!(
	wigley_shape_l_2(h),c=:black,legend=nothing);Plots.plot!(
	wigley_shape_r_2(h),c=:black,legend=nothing)

  ╠═╡ =#

# ╔═╡ 68af513d-c457-49f8-ba7c-d6ca7c142975
md"""
From the plot you can see.....



### Comparisson to other numerical methods
(Hopelijk die van kalea)
"""

# ╔═╡ 96f047dc-eb1c-4520-bad0-b4670fbafe57
md"""### Comparison between demi-hull and full hull"""

# ╔═╡ 56a91a7a-1e7f-400d-b18b-4d35f66238e8
# ╠═╡ disabled = true
#=╠═╡
panels_half, _, length_half, h_half = importMesh(joinpath(data_folder,"grid_convergence/PS_hull_0313_12-12_half_1057p.json"));
  ╠═╡ =#

# ╔═╡ cb9e519b-d5e3-440f-8d5b-bc337fe1788e
# ╠═╡ disabled = true
#=╠═╡
q_half, ps_half, A_half = solve_sources(panels_half;demi=true, Fn=0.2, verbose=true);
  ╠═╡ =#

# ╔═╡ dee1baa0-8614-465c-b232-afba4df9fe5f
# ╠═╡ disabled = true
#=╠═╡
# Plots.plot(Plots.heatmap(1 ./ A), yflip=true, colorbar_title="1/A")
  ╠═╡ =#

# ╔═╡ 8195e9a2-e85f-4e15-aba9-1096add9b77c
# ╠═╡ disabled = true
#=╠═╡
added_mass(panels_half; ps_half)
  ╠═╡ =#

# ╔═╡ d61b5b53-0ab1-4fee-8ce1-1845c54e918e
# ╠═╡ disabled = true
#=╠═╡
begin
panels_full, shape_full = importMesh(joinpath(data_folder,"PS_hull_0310_14-29_double.json"))
q_full, s = solve_sources(panels_full)
added_mass_full = added_mass(panels_full; ps_full)
end
  ╠═╡ =#

# ╔═╡ 3a72df8a-79a0-4087-beff-f839a5ddc133
# ╠═╡ disabled = true
#=╠═╡
md"""
Plot? $(@bind plot_half CheckBox(default=false))
"""
  ╠═╡ =#

# ╔═╡ 9d9ad896-0388-4802-bb0e-bc7f62db127f
# ╠═╡ disabled = true
#=╠═╡
begin
    if plot_half
	Plots.contourf(-1.5length_half:h_half:1.5length_half,-length_half:h_half:length_half,(x,y)->2ζ(x,y,q_half,panels_half;ps_half...),
	c=:balance,aspect_ratio=:equal); Plots.plot!(shape, c=:blue,legend=nothing)
    end
end

  ╠═╡ =#

# ╔═╡ 29891ae5-f88d-4618-acb2-ecf70cb4ed21
# ╠═╡ disabled = true
#=╠═╡
plot_half && plot(Plots.heatmap(0.5log.(A .^2),yflip=true,title="log.(|Aˢ|)"),
	 Plots.heatmap(0.5log.(A_half .^2),yflip=true,title="log.(|A_half|)"),
	 layout=(1,2),size=(600,300),clims=(-6,2))
  ╠═╡ =#

# ╔═╡ 42f16fad-8b1f-4292-8834-d25cd1eaa3db
# ╠═╡ disabled = true
#=╠═╡
begin
	Cwᶠ = steady_force(q, panels;ps)[1]
	Cwʰ = steady_force(q_half, panels_half; ps_half)[1]
	println("Cw full hull: $(Cwᶠ). Half hull: $(Cwʰ)")
end
  ╠═╡ =#

# ╔═╡ 8a8e28a6-0b5a-49cf-9035-97c9bb59214a
md"""
### Grid convergence
"""

# ╔═╡ beb1d70b-d5eb-4f0e-9c40-3c9abbcf63b6
# ╠═╡ disabled = true
#=╠═╡
begin
data_dir = joinpath(@__DIR__, "..", "src", "data", "grid_convergence_bigger");
files = readdir(data_dir);
resistances_full = Dict(0=>(0.0, 0.0))
resistances_half = Dict(0=>(0.0, 0.0))
for file in files
    demi = false
    if occursin("half", file)
        demi = true;
    end
	
    panels, _, _, h_mean = importMesh(joinpath(data_dir, file));
	if panels != nothing
	    q, ps, A = solve_sources(panels; demi=demi, verbose=false);
		Cw = steady_force(q, panels; ps)[1]
		demi || (resistances_full[length(panels)] = (h_mean, Cw))
		demi && (resistances_half[length(panels)] = (h_mean, Cw))
	end
end
end
  ╠═╡ =#

# ╔═╡ b40cf08b-81ef-4055-9cdc-e7ef647a4830
#=╠═╡
begin
    # Sort the data by number of panels
    sorted_full = sort(collect(filter(p -> p.first != 0, resistances_full)))
    sorted_half = sort(collect(filter(p -> p.first != 0, resistances_half)))
    
    # Extract panel counts, mean panel sizes, and resistance coefficients
    panel_counts_full = first.(sorted_full)
    h_mean_full = [p[2][1] for p in sorted_full]
    Cw_full = [p[2][2] for p in sorted_full]
    
    panel_counts_half = first.(sorted_half)
    h_mean_half = [p[2][1] for p in sorted_half]
    Cw_half = [p[2][2] for p in sorted_half]
    
    # Create main plot with panel counts as x-axis
    p = plot(
        panel_counts_full, Cw_full, 
        label="Full hull model", 
        marker=:circle,
        linestyle=:solid,
        linewidth=2,
        markersize=6,
        color=:blue,
        xlabel="Number of panels (Full ship equivalent)",
        ylabel="Wave resistance coefficient (Cw)",
        title="Grid Convergence Analysis",
        legend=:topright
    )
    
    plot!(
        panel_counts_half .* 2, Cw_half,
        label="Half hull model",
        marker=:square,
        linestyle=:dash,
        linewidth=2,
        markersize=6,
        color=:red
    )
    
    # Create a second plot with h_mean as x-axis (but will be used only for twinx)
    p2 = plot(
        h_mean_full, Cw_full,
        xlabel="Mean panel size (h_mean)",
        grid=false,
        legend=false,
        ticks=nothing,
        showaxis=false
    )
    
    # Display with both scales
    plot(p, xticks=:native)
end
  ╠═╡ =#

# ╔═╡ 2ccb150c-a71f-42eb-b228-a3f301c0fa93
# ╠═╡ disabled = true
#=╠═╡
begin
data_dir_bigger = joinpath(@__DIR__, "..", "src", "data", "grid_convergence_bigger");
full_path = joinpath(data_dir_bigger, "PS_hull_0313_17-14_double_6894p.json")
half_path = joinpath(data_dir_bigger, "PS_hull_0313_17-14_half_6193p.json")
	
panels_full_big, _, _, _ = importMesh(full_path)
panels_half_big, _, _, _ = importMesh(half_path)
	
q_full_big, ps_full_big, _ = solve_sources(panels_full_big; demi=false, verbose=false);
q_half_big, ps_half_big, _ = solve_sources(panels_half_big; demi=true, verbose=false);
	
println(added_mass(panels_full_big; ps_full_big))
println(added_mass(panels_half_big; ps_half_big))
end
  ╠═╡ =#

# ╔═╡ e315acb0-f0a0-4a2d-adc3-c510e0f46997


# ╔═╡ b00ddd51-7a31-4809-9830-05e76e2ff0f3
begin
	par_range(ref) = vcat([ref * (1 - p) for p in 0.1:0.1:0.5][end:-1:1], ref, [ref * (1 + p) for p in 0.1:0.1:0.5])
	slot_width = par_range(59)		#echte waarde
	slot_length = par_range(122)	#echte waarde	
	bow_width = par_range(32.5)	#echte waarde
	hor_br = par_range(3)			#Niet publiek bekend, schatten met rhino
	ver_br = par_range(5)			#Niet publiek bekend, schatten met rhino
	out_bk = par_range(1)			#Niet publiek bekend, schatten met rhino
	inn_bk = par_range(1)			#Niet publiek bekend, schatten met rhino

	vessel_length = 382  #CHANGE DEPENDING ON STEP 1
	compute_speeds(length; num_points=10) = round.(range(0, 1, length=num_points) .* sqrt(length * 9.81), digits=2)
	speed_range = compute_speeds(vessel_length)
	froude_range = round.(range(0, 1, length=10), digits=2)
	
	nothing
end

# ╔═╡ c45ce64f-2ae1-4120-adad-24c1f843498c
# ╠═╡ disabled = true
#=╠═╡
begin
	using DataFrames
	value = ["Speed range [m/s]", "Froude number"]
	
	speed_ranges = [speed_range, froude_range]
	
	df_speed = DataFrame(Value = value, Range = speed_ranges)
end
  ╠═╡ =#

# ╔═╡ 073d70ef-da1e-47dd-b0e5-a532b936c883
md"""
## The study

 1. **The importance of total vessel length** 

 2. **Computing the speed range and the parameter variance range for all variables**
Based on the vessel length computed above, the speed range over which each hull geometry is to be evaluated becomes:
"""

# ╔═╡ 39c8e8f6-6852-4f3e-84b3-72e8c4e3db11
md"""
Furthermore, the variance range for all parameters are displayed in the table below. For the all ranges, the middle parameter is equal to the real world value. Note that not for some parameters, the real world value is not known. These values have been visually approximated in Rhino, which is labled in the source column as "approximated".
"""

# ╔═╡ 00bf348b-9e12-4486-a772-3fe1c26234cd
# ╠═╡ disabled = true
#=╠═╡
begin
	parameters = ["Slot width", "Slot length", "Bow width", "Horizontal bow radius", "Vertical bow radius", "Outer bilge keel", "Inner bilge keel"]
	sources = ["Real world value", "Real world value", "Real world value", "Approximated", "Approximated", "Approximated", "Approximated"]
	
	ranges = [slot_width, slot_length, bow_width, hor_br, ver_br, out_bk, inn_bk]
	
	df_geometry = DataFrame(Parameter = parameters, Range = ranges, Source = sources)
end
  ╠═╡ =#

# ╔═╡ 79d035b0-df28-4e7b-bba2-f484fd24e14c
md"""
 3. **Computing the wave height for the full speed and parameter variance range**

 4. **Computing the varience in wave height caused by varying the geometric parameter**

 5. **Rank the parameters based on descending varience in wave height**

"""

# ╔═╡ 6a72863a-c5d6-46d3-b2ce-ffe420a49549
nothing

# ╔═╡ b0df71f8-b3a3-477a-b4aa-5702491840e1
md"""
## Results
"""

# ╔═╡ 0cbe5265-a2d2-45b6-bc8a-60173db020f0
nothing

# ╔═╡ 4d344c68-f99a-4df5-be6f-cdf4cff29731
md"""
## Conclusion
"""

# ╔═╡ c2437329-a343-4909-af0a-55820fcce5b3
begin
	@eval Main.PlutoRunner format_output(x::Float64; context = default_iocontext) = format_output_default(round(x; digits = 3), context)
	@eval Main.PlutoRunner format_output(x::AbstractArray{Float64}; context = default_iocontext) = format_output_default(round.(x; digits = 3), context)
end;

# ╔═╡ 9a2dc360-058b-4ba9-a477-bad65e2d2dae
# ╠═╡ disabled = true
#=╠═╡
begin
	Fnq = 0.2 	# Froude number 0.2 taken consistent across all models
	### CODE BELOW TAKEN DIRECTLY FROM WIGLEY.JL ###
	function NeumannKelvin.kelvin(ξ,α;Fn,max_z=-1/50)
		ξ[3]> 0 && throw(DomainError(ξ[3],"Sources must be below z=0"))
		x,y,z = (ξ-α)/Fn^2
		z = min(z,max_z/Fn^2) # limit z!! 💔
		(nearfield(x,y,z)+wavelike(x,abs(y),z))/Fn^2
	end
	
	∫contour(x,p;Fn) = kelvin(x,p.x .* SA[1,1,0];Fn)*p.n[1]*p.dA
	function ∫surface(x,p;Fn,χ=true,dz=0)
		(!χ || p.x[3]^2 > p.dA) && return ∫kelvin(x,p;Fn,dz) # no waterline
		∫kelvin(x,p;Fn,dz)+∫contour(x,p;Fn)
	end
	
	ps = (ϕ=∫surface,Fn=Fnq)        # NamedTuple of keyword-arguments
	q = influence(doublehull;ps...)\first.(doublehull.n); # solve for densities
	### CODE ABOVE TAKEN DIRECTLY FROM WIGLEY.JL ###
end
  ╠═╡ =#

# ╔═╡ 2adecdf9-45d8-4c18-8227-fb0a554ea3ca
# ╠═╡ disabled = true
#=╠═╡
using NeumannKelvin, Markdown, Plots
  ╠═╡ =#

# ╔═╡ 40a64ec4-cdaa-497d-9283-c3c1da062d58
# ╠═╡ disabled = true
#=╠═╡
function NeumannKelvin.kelvin(ξ,α;Fn,max_z=-1/50);
	ξ[3]> 0 && throw(DomainError(ξ[3],"Sources must be below z=0"));
	x,y,z = (ξ-α)/Fn^2;
	z = min(z,max_z/Fn^2); # limit z!! 💔
	(nearfield(x,y,z)+wavelike(x,abs(y),z))/Fn^2;
end
  ╠═╡ =#

# ╔═╡ 98e84ada-af82-4715-b894-6a9e1153ebb8
# ╠═╡ disabled = true
#=╠═╡
begin
	∫contour(x,p;Fn) = kelvin(x,p.x .* SA[1,1,0];Fn)*p.n[1]*p.dA;
	function ∫surface(x,p;Fn,χ=true,dz=0);
		(!χ || !p.wl) && return ∫kelvin(x,p;Fn,dz); # no waterline
		∫kelvin(x,p;Fn,dz)+∫contour(x,p;Fn);
	end
	function ∫surface_S₂(x,p;kwargs...);  # y-symmetric potentials
	    ∫surface(x,p;kwargs...)+∫surface(x,reflect(p,flip=SA[1,-1,1]);kwargs...);
	end
end
  ╠═╡ =#

# ╔═╡ 860dc015-a6f8-44e8-81d4-3c3391cef7dd
# ╠═╡ disabled = true
#=╠═╡
q, ps, A = solve_sources(panels;Fn=Fn1);
  ╠═╡ =#

# ╔═╡ adc7fdd3-b339-4573-9717-34d7d4ac0324
# ╠═╡ disabled = true
#=╠═╡
using NeumannKelvin, JSON, StaticArrays, LinearAlgebra, Plots, PlotlyBase,PlotlyKaleido, PlutoUI
  ╠═╡ =#

# ╔═╡ 00000000-0000-0000-0000-000000000001
PLUTO_PROJECT_TOML_CONTENTS = """
[deps]
DataFrames = "a93c6f00-e57d-5684-b7b6-d8193f3e46c0"
JSON = "682c06a0-de6a-54ab-a142-c8b1cf79cde6"
LinearAlgebra = "37e2e46d-f89d-539d-b4ee-838fcccc9c8e"
Markdown = "d6f4376e-aef5-505a-96c1-9c027394607a"
NeumannKelvin = "7f078b06-e5c4-4cf8-bb56-b92882a0ad03"
PlotlyBase = "a03496cd-edff-5a9b-9e67-9cda94a718b5"
PlotlyKaleido = "f2990250-8cf9-495f-b13a-cce12b45703c"
Plots = "91a5bcdd-55d7-5caf-9e0b-520d859cae80"
PlutoUI = "7f904dfe-b85e-4ff6-b463-dae2292396a8"
StaticArrays = "90137ffa-7385-5640-81b9-e52037218182"

[compat]
DataFrames = "~1.7.0"
JSON = "~0.21.4"
NeumannKelvin = "~0.5.1"
PlotlyBase = "~0.8.20"
PlotlyKaleido = "~2.2.6"
Plots = "~1.40.9"
PlutoUI = "~0.7.61"
StaticArrays = "~1.9.13"
"""

# ╔═╡ 00000000-0000-0000-0000-000000000002
PLUTO_MANIFEST_TOML_CONTENTS = """
# This file is machine-generated - editing it directly is not advised

julia_version = "1.11.3"
manifest_format = "2.0"
project_hash = "e504b8b5968191472ef8552fe079304f71c2ad83"

[[deps.AbstractFFTs]]
deps = ["LinearAlgebra"]
git-tree-sha1 = "d92ad398961a3ed262d8bf04a1a2b8340f915fef"
uuid = "621f4979-c628-5d54-868e-fcf4e3e8185c"
version = "1.5.0"
weakdeps = ["ChainRulesCore", "Test"]

    [deps.AbstractFFTs.extensions]
    AbstractFFTsChainRulesCoreExt = "ChainRulesCore"
    AbstractFFTsTestExt = "Test"

[[deps.AbstractPlutoDingetjes]]
deps = ["Pkg"]
git-tree-sha1 = "6e1d2a35f2f90a4bc7c2ed98079b2ba09c35b83a"
uuid = "6e696c72-6542-2067-7265-42206c756150"
version = "1.3.2"

[[deps.Accessors]]
deps = ["CompositionsBase", "ConstructionBase", "Dates", "InverseFunctions", "MacroTools"]
git-tree-sha1 = "3b86719127f50670efe356bc11073d84b4ed7a5d"
uuid = "7d9f7c33-5ae7-4f3b-8dc6-eff91059b697"
version = "0.1.42"

    [deps.Accessors.extensions]
    AxisKeysExt = "AxisKeys"
    IntervalSetsExt = "IntervalSets"
    LinearAlgebraExt = "LinearAlgebra"
    StaticArraysExt = "StaticArrays"
    StructArraysExt = "StructArrays"
    TestExt = "Test"
    UnitfulExt = "Unitful"

    [deps.Accessors.weakdeps]
    AxisKeys = "94b1ba4f-4ee9-5380-92f1-94cde586c3c5"
    IntervalSets = "8197267c-284f-5f27-9208-e0e47529a953"
    LinearAlgebra = "37e2e46d-f89d-539d-b4ee-838fcccc9c8e"
    StaticArrays = "90137ffa-7385-5640-81b9-e52037218182"
    StructArrays = "09ab397b-f2b6-538f-b94a-2f83cf4a842a"
    Test = "8dfed614-e22c-5e08-85e1-65c5234f0b40"
    Unitful = "1986cc42-f94f-5a68-af5c-568840ba703d"

[[deps.Adapt]]
deps = ["LinearAlgebra", "Requires"]
git-tree-sha1 = "cd8b948862abee8f3d3e9b73a102a9ca924debb0"
uuid = "79e6a3ab-5dfb-504d-930d-738a2a938a0e"
version = "4.2.0"
weakdeps = ["SparseArrays", "StaticArrays"]

    [deps.Adapt.extensions]
    AdaptSparseArraysExt = "SparseArrays"
    AdaptStaticArraysExt = "StaticArrays"

[[deps.AliasTables]]
deps = ["PtrArrays", "Random"]
git-tree-sha1 = "9876e1e164b144ca45e9e3198d0b689cadfed9ff"
uuid = "66dad0bd-aa9a-41b7-9441-69ab47430ed8"
version = "1.1.3"

[[deps.ArgCheck]]
git-tree-sha1 = "680b3b8759bd4c54052ada14e52355ab69e07876"
uuid = "dce04be8-c92d-5529-be00-80e4d2c0e197"
version = "2.4.0"

[[deps.ArgTools]]
uuid = "0dad84c5-d112-42e6-8d28-ef12dabb789f"
version = "1.1.2"

[[deps.Artifacts]]
uuid = "56f22d72-fd6d-98f1-02f0-08ddc0907c33"
version = "1.11.0"

[[deps.BangBang]]
deps = ["Accessors", "ConstructionBase", "InitialValues", "LinearAlgebra"]
git-tree-sha1 = "26f41e1df02c330c4fa1e98d4aa2168fdafc9b1f"
uuid = "198e06fe-97b7-11e9-32a5-e1d131e6ad66"
version = "0.4.4"

    [deps.BangBang.extensions]
    BangBangChainRulesCoreExt = "ChainRulesCore"
    BangBangDataFramesExt = "DataFrames"
    BangBangStaticArraysExt = "StaticArrays"
    BangBangStructArraysExt = "StructArrays"
    BangBangTablesExt = "Tables"
    BangBangTypedTablesExt = "TypedTables"

    [deps.BangBang.weakdeps]
    ChainRulesCore = "d360d2e6-b24c-11e9-a2a3-2a2ae2dbcce4"
    DataFrames = "a93c6f00-e57d-5684-b7b6-d8193f3e46c0"
    StaticArrays = "90137ffa-7385-5640-81b9-e52037218182"
    StructArrays = "09ab397b-f2b6-538f-b94a-2f83cf4a842a"
    Tables = "bd369af6-aec1-5ad0-b16a-f7cc5008161c"
    TypedTables = "9d95f2ec-7b3d-5a63-8d20-e2491e220bb9"

[[deps.Base64]]
uuid = "2a0f44e3-6c83-55bd-87e4-b1978d98bd5f"
version = "1.11.0"

[[deps.Baselet]]
git-tree-sha1 = "aebf55e6d7795e02ca500a689d326ac979aaf89e"
uuid = "9718e550-a3fa-408a-8086-8db961cd8217"
version = "0.1.1"

[[deps.BitFlags]]
git-tree-sha1 = "0691e34b3bb8be9307330f88d1a3c3f25466c24d"
uuid = "d1d4a3ce-64b1-5f1a-9ba4-7e7e69966f35"
version = "0.1.9"

[[deps.Bzip2_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl"]
git-tree-sha1 = "1b96ea4a01afe0ea4090c5c8039690672dd13f2e"
uuid = "6e34b625-4abd-537c-b88f-471c36dfa7a0"
version = "1.0.9+0"

[[deps.Cairo_jll]]
deps = ["Artifacts", "Bzip2_jll", "CompilerSupportLibraries_jll", "Fontconfig_jll", "FreeType2_jll", "Glib_jll", "JLLWrappers", "LZO_jll", "Libdl", "Pixman_jll", "Xorg_libXext_jll", "Xorg_libXrender_jll", "Zlib_jll", "libpng_jll"]
git-tree-sha1 = "009060c9a6168704143100f36ab08f06c2af4642"
uuid = "83423d85-b0ee-5818-9007-b63ccbeb887a"
version = "1.18.2+1"

[[deps.ChainRulesCore]]
deps = ["Compat", "LinearAlgebra"]
git-tree-sha1 = "1713c74e00545bfe14605d2a2be1712de8fbcb58"
uuid = "d360d2e6-b24c-11e9-a2a3-2a2ae2dbcce4"
version = "1.25.1"
weakdeps = ["SparseArrays"]

    [deps.ChainRulesCore.extensions]
    ChainRulesCoreSparseArraysExt = "SparseArrays"

[[deps.CodecZlib]]
deps = ["TranscodingStreams", "Zlib_jll"]
git-tree-sha1 = "962834c22b66e32aa10f7611c08c8ca4e20749a9"
uuid = "944b1d66-785c-5afd-91f1-9de20f533193"
version = "0.7.8"

[[deps.ColorSchemes]]
deps = ["ColorTypes", "ColorVectorSpace", "Colors", "FixedPointNumbers", "PrecompileTools", "Random"]
git-tree-sha1 = "403f2d8e209681fcbd9468a8514efff3ea08452e"
uuid = "35d6a980-a343-548e-a6ea-1d62b119f2f4"
version = "3.29.0"

[[deps.ColorTypes]]
deps = ["FixedPointNumbers", "Random"]
git-tree-sha1 = "b10d0b65641d57b8b4d5e234446582de5047050d"
uuid = "3da002f7-5984-5a60-b8a6-cbb66c0b333f"
version = "0.11.5"

[[deps.ColorVectorSpace]]
deps = ["ColorTypes", "FixedPointNumbers", "LinearAlgebra", "Requires", "Statistics", "TensorCore"]
git-tree-sha1 = "a1f44953f2382ebb937d60dafbe2deea4bd23249"
uuid = "c3611d14-8923-5661-9e6a-0046d554d3a4"
version = "0.10.0"
weakdeps = ["SpecialFunctions"]

    [deps.ColorVectorSpace.extensions]
    SpecialFunctionsExt = "SpecialFunctions"

[[deps.Colors]]
deps = ["ColorTypes", "FixedPointNumbers", "Reexport"]
git-tree-sha1 = "64e15186f0aa277e174aa81798f7eb8598e0157e"
uuid = "5ae59095-9a9b-59fe-a467-6f913c188581"
version = "0.13.0"

[[deps.CommonSolve]]
git-tree-sha1 = "0eee5eb66b1cf62cd6ad1b460238e60e4b09400c"
uuid = "38540f10-b2f7-11e9-35d8-d573e4eb0ff2"
version = "0.2.4"

[[deps.CommonSubexpressions]]
deps = ["MacroTools"]
git-tree-sha1 = "cda2cfaebb4be89c9084adaca7dd7333369715c5"
uuid = "bbf7d656-a473-5ed7-a52c-81e309532950"
version = "0.3.1"

[[deps.Compat]]
deps = ["TOML", "UUIDs"]
git-tree-sha1 = "8ae8d32e09f0dcf42a36b90d4e17f5dd2e4c4215"
uuid = "34da2185-b29b-5c13-b0c7-acf172513d20"
version = "4.16.0"
weakdeps = ["Dates", "LinearAlgebra"]

    [deps.Compat.extensions]
    CompatLinearAlgebraExt = "LinearAlgebra"

[[deps.CompilerSupportLibraries_jll]]
deps = ["Artifacts", "Libdl"]
uuid = "e66e0078-7015-5450-92f7-15fbd957f2ae"
version = "1.1.1+0"

[[deps.CompositionsBase]]
git-tree-sha1 = "802bb88cd69dfd1509f6670416bd4434015693ad"
uuid = "a33af91c-f02d-484b-be07-31d278c5ca2b"
version = "0.1.2"
weakdeps = ["InverseFunctions"]

    [deps.CompositionsBase.extensions]
    CompositionsBaseInverseFunctionsExt = "InverseFunctions"

[[deps.ConcurrentUtilities]]
deps = ["Serialization", "Sockets"]
git-tree-sha1 = "d9d26935a0bcffc87d2613ce14c527c99fc543fd"
uuid = "f0e56b4a-5159-44fe-b623-3e5288b988bb"
version = "2.5.0"

[[deps.ConstructionBase]]
git-tree-sha1 = "76219f1ed5771adbb096743bff43fb5fdd4c1157"
uuid = "187b0558-2788-49d3-abe0-74a17ed4e7c9"
version = "1.5.8"

    [deps.ConstructionBase.extensions]
    ConstructionBaseIntervalSetsExt = "IntervalSets"
    ConstructionBaseLinearAlgebraExt = "LinearAlgebra"
    ConstructionBaseStaticArraysExt = "StaticArrays"

    [deps.ConstructionBase.weakdeps]
    IntervalSets = "8197267c-284f-5f27-9208-e0e47529a953"
    LinearAlgebra = "37e2e46d-f89d-539d-b4ee-838fcccc9c8e"
    StaticArrays = "90137ffa-7385-5640-81b9-e52037218182"

[[deps.Contour]]
git-tree-sha1 = "439e35b0b36e2e5881738abc8857bd92ad6ff9a8"
uuid = "d38c429a-6771-53c6-b99e-75d170b6e991"
version = "0.6.3"

[[deps.Crayons]]
git-tree-sha1 = "249fe38abf76d48563e2f4556bebd215aa317e15"
uuid = "a8cc5b0e-0ffa-5ad4-8c14-923d3ee1735f"
version = "4.1.1"

[[deps.DataAPI]]
git-tree-sha1 = "abe83f3a2f1b857aac70ef8b269080af17764bbe"
uuid = "9a962f9c-6df0-11e9-0e5d-c546b8b5ee8a"
version = "1.16.0"

[[deps.DataFrames]]
deps = ["Compat", "DataAPI", "DataStructures", "Future", "InlineStrings", "InvertedIndices", "IteratorInterfaceExtensions", "LinearAlgebra", "Markdown", "Missings", "PooledArrays", "PrecompileTools", "PrettyTables", "Printf", "Random", "Reexport", "SentinelArrays", "SortingAlgorithms", "Statistics", "TableTraits", "Tables", "Unicode"]
git-tree-sha1 = "fb61b4812c49343d7ef0b533ba982c46021938a6"
uuid = "a93c6f00-e57d-5684-b7b6-d8193f3e46c0"
version = "1.7.0"

[[deps.DataStructures]]
deps = ["Compat", "InteractiveUtils", "OrderedCollections"]
git-tree-sha1 = "1d0a14036acb104d9e89698bd408f63ab58cdc82"
uuid = "864edb3b-99cc-5e75-8d2d-829cb0a9cfe8"
version = "0.18.20"

[[deps.DataValueInterfaces]]
git-tree-sha1 = "bfc1187b79289637fa0ef6d4436ebdfe6905cbd6"
uuid = "e2d170a0-9d28-54be-80f0-106bbe20a464"
version = "1.0.0"

[[deps.Dates]]
deps = ["Printf"]
uuid = "ade2ca70-3891-5945-98fb-dc099432e06a"
version = "1.11.0"

[[deps.Dbus_jll]]
deps = ["Artifacts", "Expat_jll", "JLLWrappers", "Libdl"]
git-tree-sha1 = "fc173b380865f70627d7dd1190dc2fce6cc105af"
uuid = "ee1fde0b-3d02-5ea6-8484-8dfef6360eab"
version = "1.14.10+0"

[[deps.DefineSingletons]]
git-tree-sha1 = "0fba8b706d0178b4dc7fd44a96a92382c9065c2c"
uuid = "244e2a9f-e319-4986-a169-4d1fe445cd52"
version = "0.1.2"

[[deps.DelimitedFiles]]
deps = ["Mmap"]
git-tree-sha1 = "9e2f36d3c96a820c678f2f1f1782582fcf685bae"
uuid = "8bb1440f-4735-579b-a4ab-409b98df4dab"
version = "1.9.1"

[[deps.Dictionaries]]
deps = ["Indexing", "Random", "Serialization"]
git-tree-sha1 = "1cdab237b6e0d0960d5dcbd2c0ebfa15fa6573d9"
uuid = "85a47980-9c8c-11e8-2b9f-f7ca1fa99fb4"
version = "0.4.4"

[[deps.DiffResults]]
deps = ["StaticArraysCore"]
git-tree-sha1 = "782dd5f4561f5d267313f23853baaaa4c52ea621"
uuid = "163ba53b-c6d8-5494-b064-1a9d43ac40c5"
version = "1.1.0"

[[deps.DiffRules]]
deps = ["IrrationalConstants", "LogExpFunctions", "NaNMath", "Random", "SpecialFunctions"]
git-tree-sha1 = "23163d55f885173722d1e4cf0f6110cdbaf7e272"
uuid = "b552c78f-8df3-52c6-915a-8e097449b14b"
version = "1.15.1"

[[deps.Distributed]]
deps = ["Random", "Serialization", "Sockets"]
uuid = "8ba89e20-285c-5b6f-9357-94700520ee1b"
version = "1.11.0"

[[deps.DocStringExtensions]]
deps = ["LibGit2"]
git-tree-sha1 = "2fb1e02f2b635d0845df5d7c167fec4dd739b00d"
uuid = "ffbed154-4ef7-542d-bbb7-c09d3a79fcae"
version = "0.9.3"

[[deps.Downloads]]
deps = ["ArgTools", "FileWatching", "LibCURL", "NetworkOptions"]
uuid = "f43a241f-c20a-4ad4-852c-f6b1247861c6"
version = "1.6.0"

[[deps.EpollShim_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl"]
git-tree-sha1 = "8a4be429317c42cfae6a7fc03c31bad1970c310d"
uuid = "2702e6a9-849d-5ed8-8c21-79e8b8f9ee43"
version = "0.0.20230411+1"

[[deps.ExceptionUnwrapping]]
deps = ["Test"]
git-tree-sha1 = "d36f682e590a83d63d1c7dbd287573764682d12a"
uuid = "460bff9d-24e4-43bc-9d9f-a8973cb893f4"
version = "0.1.11"

[[deps.Expat_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl"]
git-tree-sha1 = "d55dffd9ae73ff72f1c0482454dcf2ec6c6c4a63"
uuid = "2e619515-83b5-522b-bb60-26c02a35a201"
version = "2.6.5+0"

[[deps.FFMPEG]]
deps = ["FFMPEG_jll"]
git-tree-sha1 = "53ebe7511fa11d33bec688a9178fac4e49eeee00"
uuid = "c87230d0-a227-11e9-1b43-d7ebe4e7570a"
version = "0.4.2"

[[deps.FFMPEG_jll]]
deps = ["Artifacts", "Bzip2_jll", "FreeType2_jll", "FriBidi_jll", "JLLWrappers", "LAME_jll", "Libdl", "Ogg_jll", "OpenSSL_jll", "Opus_jll", "PCRE2_jll", "Zlib_jll", "libaom_jll", "libass_jll", "libfdk_aac_jll", "libvorbis_jll", "x264_jll", "x265_jll"]
git-tree-sha1 = "466d45dc38e15794ec7d5d63ec03d776a9aff36e"
uuid = "b22a6f82-2f65-5046-a5b2-351ab43fb4e5"
version = "4.4.4+1"

[[deps.FFTW]]
deps = ["AbstractFFTs", "FFTW_jll", "LinearAlgebra", "MKL_jll", "Preferences", "Reexport"]
git-tree-sha1 = "7de7c78d681078f027389e067864a8d53bd7c3c9"
uuid = "7a1cc6ca-52ef-59f5-83cd-3a7055c09341"
version = "1.8.1"

[[deps.FFTW_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Pkg"]
git-tree-sha1 = "4d81ed14783ec49ce9f2e168208a12ce1815aa25"
uuid = "f5851436-0d7a-5f13-b9de-f02708fd171a"
version = "3.3.10+3"

[[deps.FastChebInterp]]
deps = ["ChainRulesCore", "FFTW", "StaticArrays"]
git-tree-sha1 = "5b59bdc6f9517bf659ac173dac84c577fe48b0c1"
uuid = "cf66c380-9a80-432c-aff8-4f9c79c0bdde"
version = "1.2.0"

[[deps.FastGaussQuadrature]]
deps = ["LinearAlgebra", "SpecialFunctions", "StaticArrays"]
git-tree-sha1 = "fd923962364b645f3719855c88f7074413a6ad92"
uuid = "442a2c76-b920-505d-bb47-c5924d526838"
version = "1.0.2"

[[deps.FileWatching]]
uuid = "7b1f6079-737a-58dc-b8bc-7a2ca5c1b5ee"
version = "1.11.0"

[[deps.FixedPointNumbers]]
deps = ["Statistics"]
git-tree-sha1 = "05882d6995ae5c12bb5f36dd2ed3f61c98cbb172"
uuid = "53c48c17-4a7d-5ca2-90c5-79b7896eea93"
version = "0.8.5"

[[deps.Fontconfig_jll]]
deps = ["Artifacts", "Bzip2_jll", "Expat_jll", "FreeType2_jll", "JLLWrappers", "Libdl", "Libuuid_jll", "Zlib_jll"]
git-tree-sha1 = "21fac3c77d7b5a9fc03b0ec503aa1a6392c34d2b"
uuid = "a3f928ae-7b40-5064-980b-68af3947d34b"
version = "2.15.0+0"

[[deps.Format]]
git-tree-sha1 = "9c68794ef81b08086aeb32eeaf33531668d5f5fc"
uuid = "1fa38f19-a742-5d3f-a2b9-30dd87b9d5f8"
version = "1.3.7"

[[deps.ForwardDiff]]
deps = ["CommonSubexpressions", "DiffResults", "DiffRules", "LinearAlgebra", "LogExpFunctions", "NaNMath", "Preferences", "Printf", "Random", "SpecialFunctions"]
git-tree-sha1 = "a2df1b776752e3f344e5116c06d75a10436ab853"
uuid = "f6369f11-7733-5829-9624-2563aa707210"
version = "0.10.38"
weakdeps = ["StaticArrays"]

    [deps.ForwardDiff.extensions]
    ForwardDiffStaticArraysExt = "StaticArrays"

[[deps.FreeType2_jll]]
deps = ["Artifacts", "Bzip2_jll", "JLLWrappers", "Libdl", "Zlib_jll"]
git-tree-sha1 = "786e968a8d2fb167f2e4880baba62e0e26bd8e4e"
uuid = "d7e528f0-a631-5988-bf34-fe36492bcfd7"
version = "2.13.3+1"

[[deps.FriBidi_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl"]
git-tree-sha1 = "846f7026a9decf3679419122b49f8a1fdb48d2d5"
uuid = "559328eb-81f9-559d-9380-de523a88c83c"
version = "1.0.16+0"

[[deps.Future]]
deps = ["Random"]
uuid = "9fa8497b-333b-5362-9e8d-4d0656e87820"
version = "1.11.0"

[[deps.GLFW_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Libglvnd_jll", "Xorg_libXcursor_jll", "Xorg_libXi_jll", "Xorg_libXinerama_jll", "Xorg_libXrandr_jll", "libdecor_jll", "xkbcommon_jll"]
git-tree-sha1 = "fcb0584ff34e25155876418979d4c8971243bb89"
uuid = "0656b61e-2033-5cc2-a64a-77c0f6c09b89"
version = "3.4.0+2"

[[deps.GR]]
deps = ["Artifacts", "Base64", "DelimitedFiles", "Downloads", "GR_jll", "HTTP", "JSON", "Libdl", "LinearAlgebra", "Preferences", "Printf", "Qt6Wayland_jll", "Random", "Serialization", "Sockets", "TOML", "Tar", "Test", "p7zip_jll"]
git-tree-sha1 = "0ff136326605f8e06e9bcf085a356ab312eef18a"
uuid = "28b8d3ca-fb5f-59d9-8090-bfdbd6d07a71"
version = "0.73.13"

[[deps.GR_jll]]
deps = ["Artifacts", "Bzip2_jll", "Cairo_jll", "FFMPEG_jll", "Fontconfig_jll", "FreeType2_jll", "GLFW_jll", "JLLWrappers", "JpegTurbo_jll", "Libdl", "Libtiff_jll", "Pixman_jll", "Qt6Base_jll", "Zlib_jll", "libpng_jll"]
git-tree-sha1 = "9cb62849057df859575fc1dda1e91b82f8609709"
uuid = "d2c73de3-f751-5644-a686-071e5b155ba9"
version = "0.73.13+0"

[[deps.Gettext_jll]]
deps = ["Artifacts", "CompilerSupportLibraries_jll", "JLLWrappers", "Libdl", "Libiconv_jll", "Pkg", "XML2_jll"]
git-tree-sha1 = "9b02998aba7bf074d14de89f9d37ca24a1a0b046"
uuid = "78b55507-aeef-58d4-861c-77aaff3498b1"
version = "0.21.0+0"

[[deps.Glib_jll]]
deps = ["Artifacts", "Gettext_jll", "JLLWrappers", "Libdl", "Libffi_jll", "Libiconv_jll", "Libmount_jll", "PCRE2_jll", "Zlib_jll"]
git-tree-sha1 = "b0036b392358c80d2d2124746c2bf3d48d457938"
uuid = "7746bdde-850d-59dc-9ae8-88ece973131d"
version = "2.82.4+0"

[[deps.Graphite2_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Pkg"]
git-tree-sha1 = "01979f9b37367603e2848ea225918a3b3861b606"
uuid = "3b182d85-2403-5c21-9c21-1e1f0cc25472"
version = "1.3.14+1"

[[deps.Grisu]]
git-tree-sha1 = "53bb909d1151e57e2484c3d1b53e19552b887fb2"
uuid = "42e2da0e-8278-4e71-bc24-59509adca0fe"
version = "1.0.2"

[[deps.HTTP]]
deps = ["Base64", "CodecZlib", "ConcurrentUtilities", "Dates", "ExceptionUnwrapping", "Logging", "LoggingExtras", "MbedTLS", "NetworkOptions", "OpenSSL", "PrecompileTools", "Random", "SimpleBufferStream", "Sockets", "URIs", "UUIDs"]
git-tree-sha1 = "c67b33b085f6e2faf8bf79a61962e7339a81129c"
uuid = "cd3eb016-35fb-5094-929b-558a96fad6f3"
version = "1.10.15"

[[deps.HarfBuzz_jll]]
deps = ["Artifacts", "Cairo_jll", "Fontconfig_jll", "FreeType2_jll", "Glib_jll", "Graphite2_jll", "JLLWrappers", "Libdl", "Libffi_jll"]
git-tree-sha1 = "55c53be97790242c29031e5cd45e8ac296dadda3"
uuid = "2e76f6c2-a576-52d4-95c1-20adfe4de566"
version = "8.5.0+0"

[[deps.Hyperscript]]
deps = ["Test"]
git-tree-sha1 = "179267cfa5e712760cd43dcae385d7ea90cc25a4"
uuid = "47d2ed2b-36de-50cf-bf87-49c2cf4b8b91"
version = "0.0.5"

[[deps.HypertextLiteral]]
deps = ["Tricks"]
git-tree-sha1 = "7134810b1afce04bbc1045ca1985fbe81ce17653"
uuid = "ac1192a8-f4b3-4bfe-ba22-af5b92cd3ab2"
version = "0.9.5"

[[deps.IOCapture]]
deps = ["Logging", "Random"]
git-tree-sha1 = "b6d6bfdd7ce25b0f9b2f6b3dd56b2673a66c8770"
uuid = "b5f81e59-6552-4d32-b1f0-c071b021bf89"
version = "0.2.5"

[[deps.Indexing]]
git-tree-sha1 = "ce1566720fd6b19ff3411404d4b977acd4814f9f"
uuid = "313cdc1a-70c2-5d6a-ae34-0150d3930a38"
version = "1.1.1"

[[deps.InitialValues]]
git-tree-sha1 = "4da0f88e9a39111c2fa3add390ab15f3a44f3ca3"
uuid = "22cec73e-a1b8-11e9-2c92-598750a2cf9c"
version = "0.3.1"

[[deps.InlineStrings]]
git-tree-sha1 = "6a9fde685a7ac1eb3495f8e812c5a7c3711c2d5e"
uuid = "842dd82b-1e85-43dc-bf29-5d0ee9dffc48"
version = "1.4.3"

    [deps.InlineStrings.extensions]
    ArrowTypesExt = "ArrowTypes"
    ParsersExt = "Parsers"

    [deps.InlineStrings.weakdeps]
    ArrowTypes = "31f734f8-188a-4ce0-8406-c8a06bd891cd"
    Parsers = "69de0a69-1ddd-5017-9359-2bf0b02dc9f0"

[[deps.IntelOpenMP_jll]]
deps = ["Artifacts", "JLLWrappers", "LazyArtifacts", "Libdl"]
git-tree-sha1 = "0f14a5456bdc6b9731a5682f439a672750a09e48"
uuid = "1d5cc7b8-4909-519e-a0f8-d0f5ad9712d0"
version = "2025.0.4+0"

[[deps.InteractiveUtils]]
deps = ["Markdown"]
uuid = "b77e0a4c-d291-57a0-90e8-8db25a27a240"
version = "1.11.0"

[[deps.InverseFunctions]]
git-tree-sha1 = "a779299d77cd080bf77b97535acecd73e1c5e5cb"
uuid = "3587e190-3f89-42d0-90ee-14403ec27112"
version = "0.1.17"
weakdeps = ["Dates", "Test"]

    [deps.InverseFunctions.extensions]
    InverseFunctionsDatesExt = "Dates"
    InverseFunctionsTestExt = "Test"

[[deps.InvertedIndices]]
git-tree-sha1 = "6da3c4316095de0f5ee2ebd875df8721e7e0bdbe"
uuid = "41ab1584-1d38-5bbf-9106-f11c6c58b48f"
version = "1.3.1"

[[deps.IrrationalConstants]]
git-tree-sha1 = "e2222959fbc6c19554dc15174c81bf7bf3aa691c"
uuid = "92d709cd-6900-40b7-9082-c6be49f344b6"
version = "0.2.4"

[[deps.IteratorInterfaceExtensions]]
git-tree-sha1 = "a3f24677c21f5bbe9d2a714f95dcd58337fb2856"
uuid = "82899510-4779-5014-852e-03e436cf321d"
version = "1.0.0"

[[deps.JLFzf]]
deps = ["Pipe", "REPL", "Random", "fzf_jll"]
git-tree-sha1 = "71b48d857e86bf7a1838c4736545699974ce79a2"
uuid = "1019f520-868f-41f5-a6de-eb00f4b6a39c"
version = "0.1.9"

[[deps.JLLWrappers]]
deps = ["Artifacts", "Preferences"]
git-tree-sha1 = "a007feb38b422fbdab534406aeca1b86823cb4d6"
uuid = "692b3bcd-3c85-4b1f-b108-f13ce0eb3210"
version = "1.7.0"

[[deps.JSON]]
deps = ["Dates", "Mmap", "Parsers", "Unicode"]
git-tree-sha1 = "31e996f0a15c7b280ba9f76636b3ff9e2ae58c9a"
uuid = "682c06a0-de6a-54ab-a142-c8b1cf79cde6"
version = "0.21.4"

[[deps.JpegTurbo_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl"]
git-tree-sha1 = "eac1206917768cb54957c65a615460d87b455fc1"
uuid = "aacddb02-875f-59d6-b918-886e6ef4fbf8"
version = "3.1.1+0"

[[deps.Kaleido_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Pkg"]
git-tree-sha1 = "43032da5832754f58d14a91ffbe86d5f176acda9"
uuid = "f7e6163d-2fa5-5f23-b69c-1db539e41963"
version = "0.2.1+0"

[[deps.LAME_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl"]
git-tree-sha1 = "170b660facf5df5de098d866564877e119141cbd"
uuid = "c1c5ebd0-6772-5130-a774-d5fcae4a789d"
version = "3.100.2+0"

[[deps.LERC_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl"]
git-tree-sha1 = "aaafe88dccbd957a8d82f7d05be9b69172e0cee3"
uuid = "88015f11-f218-50d7-93a8-a6af411a945d"
version = "4.0.1+0"

[[deps.LLVMOpenMP_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl"]
git-tree-sha1 = "78211fb6cbc872f77cad3fc0b6cf647d923f4929"
uuid = "1d63c593-3942-5779-bab2-d838dc0a180e"
version = "18.1.7+0"

[[deps.LZO_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl"]
git-tree-sha1 = "1c602b1127f4751facb671441ca72715cc95938a"
uuid = "dd4b983a-f0e5-5f8d-a1b7-129d4a5fb1ac"
version = "2.10.3+0"

[[deps.LaTeXStrings]]
git-tree-sha1 = "dda21b8cbd6a6c40d9d02a73230f9d70fed6918c"
uuid = "b964fa9f-0449-5b57-a5c2-d3ea65f4040f"
version = "1.4.0"

[[deps.Latexify]]
deps = ["Format", "InteractiveUtils", "LaTeXStrings", "MacroTools", "Markdown", "OrderedCollections", "Requires"]
git-tree-sha1 = "cd714447457c660382fe634710fb56eb255ee42e"
uuid = "23fbe1c1-3f47-55db-b15f-69d7ec21a316"
version = "0.16.6"

    [deps.Latexify.extensions]
    DataFramesExt = "DataFrames"
    SparseArraysExt = "SparseArrays"
    SymEngineExt = "SymEngine"

    [deps.Latexify.weakdeps]
    DataFrames = "a93c6f00-e57d-5684-b7b6-d8193f3e46c0"
    SparseArrays = "2f01184e-e22b-5df5-ae63-d93ebab69eaf"
    SymEngine = "123dc426-2d89-5057-bbad-38513e3affd8"

[[deps.LazyArtifacts]]
deps = ["Artifacts", "Pkg"]
uuid = "4af54fe1-eca0-43a8-85a7-787d91b784e3"
version = "1.11.0"

[[deps.LibCURL]]
deps = ["LibCURL_jll", "MozillaCACerts_jll"]
uuid = "b27032c2-a3e7-50c8-80cd-2d36dbcbfd21"
version = "0.6.4"

[[deps.LibCURL_jll]]
deps = ["Artifacts", "LibSSH2_jll", "Libdl", "MbedTLS_jll", "Zlib_jll", "nghttp2_jll"]
uuid = "deac9b47-8bc7-5906-a0fe-35ac56dc84c0"
version = "8.6.0+0"

[[deps.LibGit2]]
deps = ["Base64", "LibGit2_jll", "NetworkOptions", "Printf", "SHA"]
uuid = "76f85450-5226-5b5a-8eaa-529ad045b433"
version = "1.11.0"

[[deps.LibGit2_jll]]
deps = ["Artifacts", "LibSSH2_jll", "Libdl", "MbedTLS_jll"]
uuid = "e37daf67-58a4-590a-8e99-b0245dd2ffc5"
version = "1.7.2+0"

[[deps.LibSSH2_jll]]
deps = ["Artifacts", "Libdl", "MbedTLS_jll"]
uuid = "29816b5a-b9ab-546f-933c-edad1886dfa8"
version = "1.11.0+1"

[[deps.Libdl]]
uuid = "8f399da3-3557-5675-b5ff-fb832c97cbdb"
version = "1.11.0"

[[deps.Libffi_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Pkg"]
git-tree-sha1 = "27ecae93dd25ee0909666e6835051dd684cc035e"
uuid = "e9f186c6-92d2-5b65-8a66-fee21dc1b490"
version = "3.2.2+2"

[[deps.Libgcrypt_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Libgpg_error_jll"]
git-tree-sha1 = "8be878062e0ffa2c3f67bb58a595375eda5de80b"
uuid = "d4300ac3-e22c-5743-9152-c294e39db1e4"
version = "1.11.0+0"

[[deps.Libglvnd_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Xorg_libX11_jll", "Xorg_libXext_jll"]
git-tree-sha1 = "ff3b4b9d35de638936a525ecd36e86a8bb919d11"
uuid = "7e76a0d4-f3c7-5321-8279-8d96eeed0f29"
version = "1.7.0+0"

[[deps.Libgpg_error_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl"]
git-tree-sha1 = "df37206100d39f79b3376afb6b9cee4970041c61"
uuid = "7add5ba3-2f88-524e-9cd5-f83b8a55f7b8"
version = "1.51.1+0"

[[deps.Libiconv_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl"]
git-tree-sha1 = "be484f5c92fad0bd8acfef35fe017900b0b73809"
uuid = "94ce4f54-9a6c-5748-9c1c-f9c7231a4531"
version = "1.18.0+0"

[[deps.Libmount_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl"]
git-tree-sha1 = "89211ea35d9df5831fca5d33552c02bd33878419"
uuid = "4b2f31a3-9ecc-558c-b454-b3730dcb73e9"
version = "2.40.3+0"

[[deps.Libtiff_jll]]
deps = ["Artifacts", "JLLWrappers", "JpegTurbo_jll", "LERC_jll", "Libdl", "XZ_jll", "Zlib_jll", "Zstd_jll"]
git-tree-sha1 = "4ab7581296671007fc33f07a721631b8855f4b1d"
uuid = "89763e89-9b03-5906-acba-b20f662cd828"
version = "4.7.1+0"

[[deps.Libuuid_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl"]
git-tree-sha1 = "e888ad02ce716b319e6bdb985d2ef300e7089889"
uuid = "38a345b3-de98-5d2b-a5d3-14cd9215e700"
version = "2.40.3+0"

[[deps.LinearAlgebra]]
deps = ["Libdl", "OpenBLAS_jll", "libblastrampoline_jll"]
uuid = "37e2e46d-f89d-539d-b4ee-838fcccc9c8e"
version = "1.11.0"

[[deps.LogExpFunctions]]
deps = ["DocStringExtensions", "IrrationalConstants", "LinearAlgebra"]
git-tree-sha1 = "13ca9e2586b89836fd20cccf56e57e2b9ae7f38f"
uuid = "2ab3a3ac-af41-5b50-aa03-7779005ae688"
version = "0.3.29"

    [deps.LogExpFunctions.extensions]
    LogExpFunctionsChainRulesCoreExt = "ChainRulesCore"
    LogExpFunctionsChangesOfVariablesExt = "ChangesOfVariables"
    LogExpFunctionsInverseFunctionsExt = "InverseFunctions"

    [deps.LogExpFunctions.weakdeps]
    ChainRulesCore = "d360d2e6-b24c-11e9-a2a3-2a2ae2dbcce4"
    ChangesOfVariables = "9e997f8a-9a97-42d5-a9f1-ce6bfc15e2c0"
    InverseFunctions = "3587e190-3f89-42d0-90ee-14403ec27112"

[[deps.Logging]]
uuid = "56ddb016-857b-54e1-b83d-db4d58db5568"
version = "1.11.0"

[[deps.LoggingExtras]]
deps = ["Dates", "Logging"]
git-tree-sha1 = "f02b56007b064fbfddb4c9cd60161b6dd0f40df3"
uuid = "e6f89c97-d47a-5376-807f-9c37f3926c36"
version = "1.1.0"

[[deps.MIMEs]]
git-tree-sha1 = "1833212fd6f580c20d4291da9c1b4e8a655b128e"
uuid = "6c6e2e6c-3030-632d-7369-2d6c69616d65"
version = "1.0.0"

[[deps.MKL_jll]]
deps = ["Artifacts", "IntelOpenMP_jll", "JLLWrappers", "LazyArtifacts", "Libdl", "oneTBB_jll"]
git-tree-sha1 = "5de60bc6cb3899cd318d80d627560fae2e2d99ae"
uuid = "856f044c-d86e-5d09-b602-aeab76dc8ba7"
version = "2025.0.1+1"

[[deps.MacroTools]]
git-tree-sha1 = "72aebe0b5051e5143a079a4685a46da330a40472"
uuid = "1914dd2f-81c6-5fcd-8719-6d5c9610ff09"
version = "0.5.15"

[[deps.Markdown]]
deps = ["Base64"]
uuid = "d6f4376e-aef5-505a-96c1-9c027394607a"
version = "1.11.0"

[[deps.MbedTLS]]
deps = ["Dates", "MbedTLS_jll", "MozillaCACerts_jll", "NetworkOptions", "Random", "Sockets"]
git-tree-sha1 = "c067a280ddc25f196b5e7df3877c6b226d390aaf"
uuid = "739be429-bea8-5141-9913-cc70e7f3736d"
version = "1.1.9"

[[deps.MbedTLS_jll]]
deps = ["Artifacts", "Libdl"]
uuid = "c8ffd9c3-330d-5841-b78e-0817d7145fa1"
version = "2.28.6+0"

[[deps.Measures]]
git-tree-sha1 = "c13304c81eec1ed3af7fc20e75fb6b26092a1102"
uuid = "442fdcdd-2543-5da2-b0f3-8c86c306513e"
version = "0.3.2"

[[deps.MicroCollections]]
deps = ["Accessors", "BangBang", "InitialValues"]
git-tree-sha1 = "44d32db644e84c75dab479f1bc15ee76a1a3618f"
uuid = "128add7d-3638-4c79-886c-908ea0c25c34"
version = "0.2.0"

[[deps.Missings]]
deps = ["DataAPI"]
git-tree-sha1 = "ec4f7fbeab05d7747bdf98eb74d130a2a2ed298d"
uuid = "e1d29d7a-bbdc-5cf2-9ac0-f12de2c33e28"
version = "1.2.0"

[[deps.Mmap]]
uuid = "a63ad114-7e13-5084-954f-fe012c677804"
version = "1.11.0"

[[deps.MozillaCACerts_jll]]
uuid = "14a3606d-f60d-562e-9121-12d972cd8159"
version = "2023.12.12"

[[deps.NaNMath]]
deps = ["OpenLibm_jll"]
git-tree-sha1 = "cc0a5deefdb12ab3a096f00a6d42133af4560d71"
uuid = "77ba4419-2d1f-58cd-9bb1-8ffee604a2e3"
version = "1.1.2"

[[deps.NetworkOptions]]
uuid = "ca575930-c2e3-43a9-ace4-1e988b2c1908"
version = "1.2.0"

[[deps.NeumannKelvin]]
deps = ["FastChebInterp", "FastGaussQuadrature", "ForwardDiff", "LinearAlgebra", "QuadGK", "Reexport", "Roots", "SpecialFunctions", "StaticArrays", "ThreadsX", "TypedTables"]
git-tree-sha1 = "716e5271e10f9c4fd5d1c6b07ec690d7903606c9"
uuid = "7f078b06-e5c4-4cf8-bb56-b92882a0ad03"
version = "0.5.1"

[[deps.Ogg_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Pkg"]
git-tree-sha1 = "887579a3eb005446d514ab7aeac5d1d027658b8f"
uuid = "e7412a2a-1a6e-54c0-be00-318e2571c051"
version = "1.3.5+1"

[[deps.OpenBLAS_jll]]
deps = ["Artifacts", "CompilerSupportLibraries_jll", "Libdl"]
uuid = "4536629a-c528-5b80-bd46-f80d51c5b363"
version = "0.3.27+1"

[[deps.OpenLibm_jll]]
deps = ["Artifacts", "Libdl"]
uuid = "05823500-19ac-5b8b-9628-191a04bc5112"
version = "0.8.1+2"

[[deps.OpenSSL]]
deps = ["BitFlags", "Dates", "MozillaCACerts_jll", "OpenSSL_jll", "Sockets"]
git-tree-sha1 = "38cb508d080d21dc1128f7fb04f20387ed4c0af4"
uuid = "4d8831e6-92b7-49fb-bdf8-b643e874388c"
version = "1.4.3"

[[deps.OpenSSL_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl"]
git-tree-sha1 = "a9697f1d06cc3eb3fb3ad49cc67f2cfabaac31ea"
uuid = "458c3c95-2e84-50aa-8efc-19380b2a3a95"
version = "3.0.16+0"

[[deps.OpenSpecFun_jll]]
deps = ["Artifacts", "CompilerSupportLibraries_jll", "JLLWrappers", "Libdl"]
git-tree-sha1 = "1346c9208249809840c91b26703912dff463d335"
uuid = "efe28fd5-8261-553b-a9e1-b2916fc3738e"
version = "0.5.6+0"

[[deps.Opus_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl"]
git-tree-sha1 = "6703a85cb3781bd5909d48730a67205f3f31a575"
uuid = "91d4177d-7536-5919-b921-800302f37372"
version = "1.3.3+0"

[[deps.OrderedCollections]]
git-tree-sha1 = "cc4054e898b852042d7b503313f7ad03de99c3dd"
uuid = "bac558e1-5e72-5ebc-8fee-abe8a469f55d"
version = "1.8.0"

[[deps.PCRE2_jll]]
deps = ["Artifacts", "Libdl"]
uuid = "efcefdf7-47ab-520b-bdef-62a2eaa19f15"
version = "10.42.0+1"

[[deps.Pango_jll]]
deps = ["Artifacts", "Cairo_jll", "Fontconfig_jll", "FreeType2_jll", "FriBidi_jll", "Glib_jll", "HarfBuzz_jll", "JLLWrappers", "Libdl"]
git-tree-sha1 = "3b31172c032a1def20c98dae3f2cdc9d10e3b561"
uuid = "36c8627f-9965-5494-a995-c6b170f724f3"
version = "1.56.1+0"

[[deps.Parameters]]
deps = ["OrderedCollections", "UnPack"]
git-tree-sha1 = "34c0e9ad262e5f7fc75b10a9952ca7692cfc5fbe"
uuid = "d96e819e-fc66-5662-9728-84c9c7592b0a"
version = "0.12.3"

[[deps.Parsers]]
deps = ["Dates", "PrecompileTools", "UUIDs"]
git-tree-sha1 = "8489905bcdbcfac64d1daa51ca07c0d8f0283821"
uuid = "69de0a69-1ddd-5017-9359-2bf0b02dc9f0"
version = "2.8.1"

[[deps.Pipe]]
git-tree-sha1 = "6842804e7867b115ca9de748a0cf6b364523c16d"
uuid = "b98c9c47-44ae-5843-9183-064241ee97a0"
version = "1.3.0"

[[deps.Pixman_jll]]
deps = ["Artifacts", "CompilerSupportLibraries_jll", "JLLWrappers", "LLVMOpenMP_jll", "Libdl"]
git-tree-sha1 = "35621f10a7531bc8fa58f74610b1bfb70a3cfc6b"
uuid = "30392449-352a-5448-841d-b1acce4e97dc"
version = "0.43.4+0"

[[deps.Pkg]]
deps = ["Artifacts", "Dates", "Downloads", "FileWatching", "LibGit2", "Libdl", "Logging", "Markdown", "Printf", "Random", "SHA", "TOML", "Tar", "UUIDs", "p7zip_jll"]
uuid = "44cfe95a-1eb2-52ea-b672-e2afdf69b78f"
version = "1.11.0"
weakdeps = ["REPL"]

    [deps.Pkg.extensions]
    REPLExt = "REPL"

[[deps.PlotThemes]]
deps = ["PlotUtils", "Statistics"]
git-tree-sha1 = "41031ef3a1be6f5bbbf3e8073f210556daeae5ca"
uuid = "ccf2f8ad-2431-5c83-bf29-c5338b663b6a"
version = "3.3.0"

[[deps.PlotUtils]]
deps = ["ColorSchemes", "Colors", "Dates", "PrecompileTools", "Printf", "Random", "Reexport", "StableRNGs", "Statistics"]
git-tree-sha1 = "3ca9a356cd2e113c420f2c13bea19f8d3fb1cb18"
uuid = "995b91a9-d308-5afd-9ec6-746e21dbc043"
version = "1.4.3"

[[deps.PlotlyBase]]
deps = ["ColorSchemes", "Colors", "Dates", "DelimitedFiles", "DocStringExtensions", "JSON", "LaTeXStrings", "Logging", "Parameters", "Pkg", "REPL", "Requires", "Statistics", "UUIDs"]
git-tree-sha1 = "90af5c9238c1b3b25421f1fdfffd1e8fca7a7133"
uuid = "a03496cd-edff-5a9b-9e67-9cda94a718b5"
version = "0.8.20"

    [deps.PlotlyBase.extensions]
    DataFramesExt = "DataFrames"
    DistributionsExt = "Distributions"
    IJuliaExt = "IJulia"
    JSON3Ext = "JSON3"

    [deps.PlotlyBase.weakdeps]
    DataFrames = "a93c6f00-e57d-5684-b7b6-d8193f3e46c0"
    Distributions = "31c24e10-a181-5473-b8eb-7969acd0382f"
    IJulia = "7073ff75-c697-5162-941a-fcdaad2a7d2a"
    JSON3 = "0f8b85d8-7281-11e9-16c2-39a750bddbf1"

[[deps.PlotlyKaleido]]
deps = ["Artifacts", "Base64", "JSON", "Kaleido_jll"]
git-tree-sha1 = "ba551e47d7eac212864fdfea3bd07f30202b4a5b"
uuid = "f2990250-8cf9-495f-b13a-cce12b45703c"
version = "2.2.6"

[[deps.Plots]]
deps = ["Base64", "Contour", "Dates", "Downloads", "FFMPEG", "FixedPointNumbers", "GR", "JLFzf", "JSON", "LaTeXStrings", "Latexify", "LinearAlgebra", "Measures", "NaNMath", "Pkg", "PlotThemes", "PlotUtils", "PrecompileTools", "Printf", "REPL", "Random", "RecipesBase", "RecipesPipeline", "Reexport", "RelocatableFolders", "Requires", "Scratch", "Showoff", "SparseArrays", "Statistics", "StatsBase", "TOML", "UUIDs", "UnicodeFun", "UnitfulLatexify", "Unzip"]
git-tree-sha1 = "dae01f8c2e069a683d3a6e17bbae5070ab94786f"
uuid = "91a5bcdd-55d7-5caf-9e0b-520d859cae80"
version = "1.40.9"

    [deps.Plots.extensions]
    FileIOExt = "FileIO"
    GeometryBasicsExt = "GeometryBasics"
    IJuliaExt = "IJulia"
    ImageInTerminalExt = "ImageInTerminal"
    UnitfulExt = "Unitful"

    [deps.Plots.weakdeps]
    FileIO = "5789e2e9-d7fb-5bc7-8068-2c6fae9b9549"
    GeometryBasics = "5c1252a2-5f33-56bf-86c9-59e7332b4326"
    IJulia = "7073ff75-c697-5162-941a-fcdaad2a7d2a"
    ImageInTerminal = "d8c32880-2388-543b-8c61-d9f865259254"
    Unitful = "1986cc42-f94f-5a68-af5c-568840ba703d"

[[deps.PlutoUI]]
deps = ["AbstractPlutoDingetjes", "Base64", "ColorTypes", "Dates", "FixedPointNumbers", "Hyperscript", "HypertextLiteral", "IOCapture", "InteractiveUtils", "JSON", "Logging", "MIMEs", "Markdown", "Random", "Reexport", "URIs", "UUIDs"]
git-tree-sha1 = "7e71a55b87222942f0f9337be62e26b1f103d3e4"
uuid = "7f904dfe-b85e-4ff6-b463-dae2292396a8"
version = "0.7.61"

[[deps.PooledArrays]]
deps = ["DataAPI", "Future"]
git-tree-sha1 = "36d8b4b899628fb92c2749eb488d884a926614d3"
uuid = "2dfb63ee-cc39-5dd5-95bd-886bf059d720"
version = "1.4.3"

[[deps.PrecompileTools]]
deps = ["Preferences"]
git-tree-sha1 = "5aa36f7049a63a1528fe8f7c3f2113413ffd4e1f"
uuid = "aea7be01-6a6a-4083-8856-8a6e6704d82a"
version = "1.2.1"

[[deps.Preferences]]
deps = ["TOML"]
git-tree-sha1 = "9306f6085165d270f7e3db02af26a400d580f5c6"
uuid = "21216c6a-2e73-6563-6e65-726566657250"
version = "1.4.3"

[[deps.PrettyTables]]
deps = ["Crayons", "LaTeXStrings", "Markdown", "PrecompileTools", "Printf", "Reexport", "StringManipulation", "Tables"]
git-tree-sha1 = "1101cd475833706e4d0e7b122218257178f48f34"
uuid = "08abe8d2-0d0c-5749-adfa-8a2ac140af0d"
version = "2.4.0"

[[deps.Printf]]
deps = ["Unicode"]
uuid = "de0858da-6303-5e67-8744-51eddeeeb8d7"
version = "1.11.0"

[[deps.PtrArrays]]
git-tree-sha1 = "1d36ef11a9aaf1e8b74dacc6a731dd1de8fd493d"
uuid = "43287f4e-b6f4-7ad1-bb20-aadabca52c3d"
version = "1.3.0"

[[deps.Qt6Base_jll]]
deps = ["Artifacts", "CompilerSupportLibraries_jll", "Fontconfig_jll", "Glib_jll", "JLLWrappers", "Libdl", "Libglvnd_jll", "OpenSSL_jll", "Vulkan_Loader_jll", "Xorg_libSM_jll", "Xorg_libXext_jll", "Xorg_libXrender_jll", "Xorg_libxcb_jll", "Xorg_xcb_util_cursor_jll", "Xorg_xcb_util_image_jll", "Xorg_xcb_util_keysyms_jll", "Xorg_xcb_util_renderutil_jll", "Xorg_xcb_util_wm_jll", "Zlib_jll", "libinput_jll", "xkbcommon_jll"]
git-tree-sha1 = "492601870742dcd38f233b23c3ec629628c1d724"
uuid = "c0090381-4147-56d7-9ebc-da0b1113ec56"
version = "6.7.1+1"

[[deps.Qt6Declarative_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Qt6Base_jll", "Qt6ShaderTools_jll"]
git-tree-sha1 = "e5dd466bf2569fe08c91a2cc29c1003f4797ac3b"
uuid = "629bc702-f1f5-5709-abd5-49b8460ea067"
version = "6.7.1+2"

[[deps.Qt6ShaderTools_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Qt6Base_jll"]
git-tree-sha1 = "1a180aeced866700d4bebc3120ea1451201f16bc"
uuid = "ce943373-25bb-56aa-8eca-768745ed7b5a"
version = "6.7.1+1"

[[deps.Qt6Wayland_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Qt6Base_jll", "Qt6Declarative_jll"]
git-tree-sha1 = "729927532d48cf79f49070341e1d918a65aba6b0"
uuid = "e99dba38-086e-5de3-a5b1-6e4c66e897c3"
version = "6.7.1+1"

[[deps.QuadGK]]
deps = ["DataStructures", "LinearAlgebra"]
git-tree-sha1 = "9da16da70037ba9d701192e27befedefb91ec284"
uuid = "1fd47b50-473d-5c70-9696-f719f8f3bcdc"
version = "2.11.2"

    [deps.QuadGK.extensions]
    QuadGKEnzymeExt = "Enzyme"

    [deps.QuadGK.weakdeps]
    Enzyme = "7da242da-08ed-463a-9acd-ee780be4f1d9"

[[deps.REPL]]
deps = ["InteractiveUtils", "Markdown", "Sockets", "StyledStrings", "Unicode"]
uuid = "3fa0cd96-eef1-5676-8a61-b3b8758bbffb"
version = "1.11.0"

[[deps.Random]]
deps = ["SHA"]
uuid = "9a3f8284-a2c9-5f02-9a11-845980a1fd5c"
version = "1.11.0"

[[deps.RecipesBase]]
deps = ["PrecompileTools"]
git-tree-sha1 = "5c3d09cc4f31f5fc6af001c250bf1278733100ff"
uuid = "3cdcf5f2-1ef4-517c-9805-6587b60abb01"
version = "1.3.4"

[[deps.RecipesPipeline]]
deps = ["Dates", "NaNMath", "PlotUtils", "PrecompileTools", "RecipesBase"]
git-tree-sha1 = "45cf9fd0ca5839d06ef333c8201714e888486342"
uuid = "01d81517-befc-4cb6-b9ec-a95719d0359c"
version = "0.6.12"

[[deps.Reexport]]
git-tree-sha1 = "45e428421666073eab6f2da5c9d310d99bb12f9b"
uuid = "189a3867-3050-52da-a836-e630ba90ab69"
version = "1.2.2"

[[deps.Referenceables]]
deps = ["Adapt"]
git-tree-sha1 = "02d31ad62838181c1a3a5fd23a1ce5914a643601"
uuid = "42d2dcc6-99eb-4e98-b66c-637b7d73030e"
version = "0.1.3"

[[deps.RelocatableFolders]]
deps = ["SHA", "Scratch"]
git-tree-sha1 = "ffdaf70d81cf6ff22c2b6e733c900c3321cab864"
uuid = "05181044-ff0b-4ac5-8273-598c1e38db00"
version = "1.0.1"

[[deps.Requires]]
deps = ["UUIDs"]
git-tree-sha1 = "62389eeff14780bfe55195b7204c0d8738436d64"
uuid = "ae029012-a4dd-5104-9daa-d747884805df"
version = "1.3.1"

[[deps.Roots]]
deps = ["Accessors", "CommonSolve", "Printf"]
git-tree-sha1 = "442b4353ee8c26756672afb2db81894fc28811f3"
uuid = "f2b01f46-fcfa-551c-844a-d8ac1e96c665"
version = "2.2.6"

    [deps.Roots.extensions]
    RootsChainRulesCoreExt = "ChainRulesCore"
    RootsForwardDiffExt = "ForwardDiff"
    RootsIntervalRootFindingExt = "IntervalRootFinding"
    RootsSymPyExt = "SymPy"
    RootsSymPyPythonCallExt = "SymPyPythonCall"

    [deps.Roots.weakdeps]
    ChainRulesCore = "d360d2e6-b24c-11e9-a2a3-2a2ae2dbcce4"
    ForwardDiff = "f6369f11-7733-5829-9624-2563aa707210"
    IntervalRootFinding = "d2bf35a9-74e0-55ec-b149-d360ff49b807"
    SymPy = "24249f21-da20-56a4-8eb1-6a02cf4ae2e6"
    SymPyPythonCall = "bc8888f7-b21e-4b7c-a06a-5d9c9496438c"

[[deps.SHA]]
uuid = "ea8e919c-243c-51af-8825-aaa63cd721ce"
version = "0.7.0"

[[deps.Scratch]]
deps = ["Dates"]
git-tree-sha1 = "3bac05bc7e74a75fd9cba4295cde4045d9fe2386"
uuid = "6c6a2e73-6563-6170-7368-637461726353"
version = "1.2.1"

[[deps.SentinelArrays]]
deps = ["Dates", "Random"]
git-tree-sha1 = "712fb0231ee6f9120e005ccd56297abbc053e7e0"
uuid = "91c51154-3ec4-41a3-a24f-3f23e20d615c"
version = "1.4.8"

[[deps.Serialization]]
uuid = "9e88b42a-f829-5b0c-bbe9-9e923198166b"
version = "1.11.0"

[[deps.Setfield]]
deps = ["ConstructionBase", "Future", "MacroTools", "StaticArraysCore"]
git-tree-sha1 = "c5391c6ace3bc430ca630251d02ea9687169ca68"
uuid = "efcf1570-3423-57d1-acb7-fd33fddbac46"
version = "1.1.2"

[[deps.Showoff]]
deps = ["Dates", "Grisu"]
git-tree-sha1 = "91eddf657aca81df9ae6ceb20b959ae5653ad1de"
uuid = "992d4aef-0814-514b-bc4d-f2e9a6c4116f"
version = "1.0.3"

[[deps.SimpleBufferStream]]
git-tree-sha1 = "f305871d2f381d21527c770d4788c06c097c9bc1"
uuid = "777ac1f9-54b0-4bf8-805c-2214025038e7"
version = "1.2.0"

[[deps.Sockets]]
uuid = "6462fe0b-24de-5631-8697-dd941f90decc"
version = "1.11.0"

[[deps.SortingAlgorithms]]
deps = ["DataStructures"]
git-tree-sha1 = "66e0a8e672a0bdfca2c3f5937efb8538b9ddc085"
uuid = "a2af1166-a08f-5f64-846c-94a0d3cef48c"
version = "1.2.1"

[[deps.SparseArrays]]
deps = ["Libdl", "LinearAlgebra", "Random", "Serialization", "SuiteSparse_jll"]
uuid = "2f01184e-e22b-5df5-ae63-d93ebab69eaf"
version = "1.11.0"

[[deps.SpecialFunctions]]
deps = ["IrrationalConstants", "LogExpFunctions", "OpenLibm_jll", "OpenSpecFun_jll"]
git-tree-sha1 = "64cca0c26b4f31ba18f13f6c12af7c85f478cfde"
uuid = "276daf66-3868-5448-9aa4-cd146d93841b"
version = "2.5.0"
weakdeps = ["ChainRulesCore"]

    [deps.SpecialFunctions.extensions]
    SpecialFunctionsChainRulesCoreExt = "ChainRulesCore"

[[deps.SplitApplyCombine]]
deps = ["Dictionaries", "Indexing"]
git-tree-sha1 = "c06d695d51cfb2187e6848e98d6252df9101c588"
uuid = "03a91e81-4c3e-53e1-a0a4-9c0c8f19dd66"
version = "1.2.3"

[[deps.SplittablesBase]]
deps = ["Setfield", "Test"]
git-tree-sha1 = "e08a62abc517eb79667d0a29dc08a3b589516bb5"
uuid = "171d559e-b47b-412a-8079-5efa626c420e"
version = "0.1.15"

[[deps.StableRNGs]]
deps = ["Random"]
git-tree-sha1 = "83e6cce8324d49dfaf9ef059227f91ed4441a8e5"
uuid = "860ef19b-820b-49d6-a774-d7a799459cd3"
version = "1.0.2"

[[deps.StaticArrays]]
deps = ["LinearAlgebra", "PrecompileTools", "Random", "StaticArraysCore"]
git-tree-sha1 = "0feb6b9031bd5c51f9072393eb5ab3efd31bf9e4"
uuid = "90137ffa-7385-5640-81b9-e52037218182"
version = "1.9.13"
weakdeps = ["ChainRulesCore", "Statistics"]

    [deps.StaticArrays.extensions]
    StaticArraysChainRulesCoreExt = "ChainRulesCore"
    StaticArraysStatisticsExt = "Statistics"

[[deps.StaticArraysCore]]
git-tree-sha1 = "192954ef1208c7019899fbf8049e717f92959682"
uuid = "1e83bf80-4336-4d27-bf5d-d5a4f845583c"
version = "1.4.3"

[[deps.Statistics]]
deps = ["LinearAlgebra"]
git-tree-sha1 = "ae3bb1eb3bba077cd276bc5cfc337cc65c3075c0"
uuid = "10745b16-79ce-11e8-11f9-7d13ad32a3b2"
version = "1.11.1"
weakdeps = ["SparseArrays"]

    [deps.Statistics.extensions]
    SparseArraysExt = ["SparseArrays"]

[[deps.StatsAPI]]
deps = ["LinearAlgebra"]
git-tree-sha1 = "1ff449ad350c9c4cbc756624d6f8a8c3ef56d3ed"
uuid = "82ae8749-77ed-4fe6-ae5f-f523153014b0"
version = "1.7.0"

[[deps.StatsBase]]
deps = ["AliasTables", "DataAPI", "DataStructures", "LinearAlgebra", "LogExpFunctions", "Missings", "Printf", "Random", "SortingAlgorithms", "SparseArrays", "Statistics", "StatsAPI"]
git-tree-sha1 = "29321314c920c26684834965ec2ce0dacc9cf8e5"
uuid = "2913bbd2-ae8a-5f71-8c99-4fb6c76f3a91"
version = "0.34.4"

[[deps.StringManipulation]]
deps = ["PrecompileTools"]
git-tree-sha1 = "725421ae8e530ec29bcbdddbe91ff8053421d023"
uuid = "892a3eda-7b42-436c-8928-eab12a02cf0e"
version = "0.4.1"

[[deps.StyledStrings]]
uuid = "f489334b-da3d-4c2e-b8f0-e476e12c162b"
version = "1.11.0"

[[deps.SuiteSparse_jll]]
deps = ["Artifacts", "Libdl", "libblastrampoline_jll"]
uuid = "bea87d4a-7f5b-5778-9afe-8cc45184846c"
version = "7.7.0+0"

[[deps.TOML]]
deps = ["Dates"]
uuid = "fa267f1f-6049-4f14-aa54-33bafae1ed76"
version = "1.0.3"

[[deps.TableTraits]]
deps = ["IteratorInterfaceExtensions"]
git-tree-sha1 = "c06b2f539df1c6efa794486abfb6ed2022561a39"
uuid = "3783bdb8-4a98-5b6b-af9a-565f29a5fe9c"
version = "1.0.1"

[[deps.Tables]]
deps = ["DataAPI", "DataValueInterfaces", "IteratorInterfaceExtensions", "OrderedCollections", "TableTraits"]
git-tree-sha1 = "598cd7c1f68d1e205689b1c2fe65a9f85846f297"
uuid = "bd369af6-aec1-5ad0-b16a-f7cc5008161c"
version = "1.12.0"

[[deps.Tar]]
deps = ["ArgTools", "SHA"]
uuid = "a4e569a6-e804-4fa4-b0f3-eef7a1d5b13e"
version = "1.10.0"

[[deps.TensorCore]]
deps = ["LinearAlgebra"]
git-tree-sha1 = "1feb45f88d133a655e001435632f019a9a1bcdb6"
uuid = "62fd8b95-f654-4bbd-a8a5-9c27f68ccd50"
version = "0.1.1"

[[deps.Test]]
deps = ["InteractiveUtils", "Logging", "Random", "Serialization"]
uuid = "8dfed614-e22c-5e08-85e1-65c5234f0b40"
version = "1.11.0"

[[deps.ThreadsX]]
deps = ["Accessors", "ArgCheck", "BangBang", "ConstructionBase", "InitialValues", "MicroCollections", "Referenceables", "SplittablesBase", "Transducers"]
git-tree-sha1 = "70bd8244f4834d46c3d68bd09e7792d8f571ef04"
uuid = "ac1d9e8a-700a-412c-b207-f0111f4b6c0d"
version = "0.1.12"

[[deps.TranscodingStreams]]
git-tree-sha1 = "0c45878dcfdcfa8480052b6ab162cdd138781742"
uuid = "3bb67fe8-82b1-5028-8e26-92a6c54297fa"
version = "0.11.3"

[[deps.Transducers]]
deps = ["Accessors", "ArgCheck", "BangBang", "Baselet", "CompositionsBase", "ConstructionBase", "DefineSingletons", "Distributed", "InitialValues", "Logging", "Markdown", "MicroCollections", "Requires", "SplittablesBase", "Tables"]
git-tree-sha1 = "7deeab4ff96b85c5f72c824cae53a1398da3d1cb"
uuid = "28d57a85-8fef-5791-bfe6-a80928e7c999"
version = "0.4.84"

    [deps.Transducers.extensions]
    TransducersAdaptExt = "Adapt"
    TransducersBlockArraysExt = "BlockArrays"
    TransducersDataFramesExt = "DataFrames"
    TransducersLazyArraysExt = "LazyArrays"
    TransducersOnlineStatsBaseExt = "OnlineStatsBase"
    TransducersReferenceablesExt = "Referenceables"

    [deps.Transducers.weakdeps]
    Adapt = "79e6a3ab-5dfb-504d-930d-738a2a938a0e"
    BlockArrays = "8e7c35d0-a365-5155-bbbb-fb81a777f24e"
    DataFrames = "a93c6f00-e57d-5684-b7b6-d8193f3e46c0"
    LazyArrays = "5078a376-72f3-5289-bfd5-ec5146d43c02"
    OnlineStatsBase = "925886fa-5bf2-5e8e-b522-a9147a512338"
    Referenceables = "42d2dcc6-99eb-4e98-b66c-637b7d73030e"

[[deps.Tricks]]
git-tree-sha1 = "6cae795a5a9313bbb4f60683f7263318fc7d1505"
uuid = "410a4b4d-49e4-4fbc-ab6d-cb71b17b3775"
version = "0.1.10"

[[deps.TypedTables]]
deps = ["Adapt", "Dictionaries", "Indexing", "SplitApplyCombine", "Tables", "Unicode"]
git-tree-sha1 = "84fd7dadde577e01eb4323b7e7b9cb51c62c60d4"
uuid = "9d95f2ec-7b3d-5a63-8d20-e2491e220bb9"
version = "1.4.6"

[[deps.URIs]]
git-tree-sha1 = "67db6cc7b3821e19ebe75791a9dd19c9b1188f2b"
uuid = "5c2747f8-b7ea-4ff2-ba2e-563bfd36b1d4"
version = "1.5.1"

[[deps.UUIDs]]
deps = ["Random", "SHA"]
uuid = "cf7118a7-6976-5b1a-9a39-7adc72f591a4"
version = "1.11.0"

[[deps.UnPack]]
git-tree-sha1 = "387c1f73762231e86e0c9c5443ce3b4a0a9a0c2b"
uuid = "3a884ed6-31ef-47d7-9d2a-63182c4928ed"
version = "1.0.2"

[[deps.Unicode]]
uuid = "4ec0a83e-493e-50e2-b9ac-8f72acf5a8f5"
version = "1.11.0"

[[deps.UnicodeFun]]
deps = ["REPL"]
git-tree-sha1 = "53915e50200959667e78a92a418594b428dffddf"
uuid = "1cfade01-22cf-5700-b092-accc4b62d6e1"
version = "0.4.1"

[[deps.Unitful]]
deps = ["Dates", "LinearAlgebra", "Random"]
git-tree-sha1 = "c0667a8e676c53d390a09dc6870b3d8d6650e2bf"
uuid = "1986cc42-f94f-5a68-af5c-568840ba703d"
version = "1.22.0"
weakdeps = ["ConstructionBase", "InverseFunctions"]

    [deps.Unitful.extensions]
    ConstructionBaseUnitfulExt = "ConstructionBase"
    InverseFunctionsUnitfulExt = "InverseFunctions"

[[deps.UnitfulLatexify]]
deps = ["LaTeXStrings", "Latexify", "Unitful"]
git-tree-sha1 = "975c354fcd5f7e1ddcc1f1a23e6e091d99e99bc8"
uuid = "45397f5d-5981-4c77-b2b3-fc36d6e9b728"
version = "1.6.4"

[[deps.Unzip]]
git-tree-sha1 = "ca0969166a028236229f63514992fc073799bb78"
uuid = "41fe7b60-77ed-43a1-b4f0-825fd5a5650d"
version = "0.2.0"

[[deps.Vulkan_Loader_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Wayland_jll", "Xorg_libX11_jll", "Xorg_libXrandr_jll", "xkbcommon_jll"]
git-tree-sha1 = "2f0486047a07670caad3a81a075d2e518acc5c59"
uuid = "a44049a8-05dd-5a78-86c9-5fde0876e88c"
version = "1.3.243+0"

[[deps.Wayland_jll]]
deps = ["Artifacts", "EpollShim_jll", "Expat_jll", "JLLWrappers", "Libdl", "Libffi_jll", "Pkg", "XML2_jll"]
git-tree-sha1 = "85c7811eddec9e7f22615371c3cc81a504c508ee"
uuid = "a2964d1f-97da-50d4-b82a-358c7fce9d89"
version = "1.21.0+2"

[[deps.Wayland_protocols_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Pkg"]
git-tree-sha1 = "5db3e9d307d32baba7067b13fc7b5aa6edd4a19a"
uuid = "2381bf8a-dfd0-557d-9999-79630e7b1b91"
version = "1.36.0+0"

[[deps.XML2_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Libiconv_jll", "Zlib_jll"]
git-tree-sha1 = "b8b243e47228b4a3877f1dd6aee0c5d56db7fcf4"
uuid = "02c8fc9c-b97f-50b9-bbe4-9be30ff0a78a"
version = "2.13.6+1"

[[deps.XSLT_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Libgcrypt_jll", "Libgpg_error_jll", "Libiconv_jll", "XML2_jll", "Zlib_jll"]
git-tree-sha1 = "7d1671acbe47ac88e981868a078bd6b4e27c5191"
uuid = "aed1982a-8fda-507f-9586-7b0439959a61"
version = "1.1.42+0"

[[deps.XZ_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl"]
git-tree-sha1 = "56c6604ec8b2d82cc4cfe01aa03b00426aac7e1f"
uuid = "ffd25f8a-64ca-5728-b0f7-c24cf3aae800"
version = "5.6.4+1"

[[deps.Xorg_libICE_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl"]
git-tree-sha1 = "326b4fea307b0b39892b3e85fa451692eda8d46c"
uuid = "f67eecfb-183a-506d-b269-f58e52b52d7c"
version = "1.1.1+0"

[[deps.Xorg_libSM_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Xorg_libICE_jll"]
git-tree-sha1 = "3796722887072218eabafb494a13c963209754ce"
uuid = "c834827a-8449-5923-a945-d239c165b7dd"
version = "1.2.4+0"

[[deps.Xorg_libX11_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Xorg_libxcb_jll", "Xorg_xtrans_jll"]
git-tree-sha1 = "9dafcee1d24c4f024e7edc92603cedba72118283"
uuid = "4f6342f7-b3d2-589e-9d20-edeb45f2b2bc"
version = "1.8.6+3"

[[deps.Xorg_libXau_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl"]
git-tree-sha1 = "e9216fdcd8514b7072b43653874fd688e4c6c003"
uuid = "0c0b7dd1-d40b-584c-a123-a41640f87eec"
version = "1.0.12+0"

[[deps.Xorg_libXcursor_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Xorg_libXfixes_jll", "Xorg_libXrender_jll"]
git-tree-sha1 = "807c226eaf3651e7b2c468f687ac788291f9a89b"
uuid = "935fb764-8cf2-53bf-bb30-45bb1f8bf724"
version = "1.2.3+0"

[[deps.Xorg_libXdmcp_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl"]
git-tree-sha1 = "89799ae67c17caa5b3b5a19b8469eeee474377db"
uuid = "a3789734-cfe1-5b06-b2d0-1dd0d9d62d05"
version = "1.1.5+0"

[[deps.Xorg_libXext_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Xorg_libX11_jll"]
git-tree-sha1 = "d7155fea91a4123ef59f42c4afb5ab3b4ca95058"
uuid = "1082639a-0dae-5f34-9b06-72781eeb8cb3"
version = "1.3.6+3"

[[deps.Xorg_libXfixes_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Xorg_libX11_jll"]
git-tree-sha1 = "6fcc21d5aea1a0b7cce6cab3e62246abd1949b86"
uuid = "d091e8ba-531a-589c-9de9-94069b037ed8"
version = "6.0.0+0"

[[deps.Xorg_libXi_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Xorg_libXext_jll", "Xorg_libXfixes_jll"]
git-tree-sha1 = "984b313b049c89739075b8e2a94407076de17449"
uuid = "a51aa0fd-4e3c-5386-b890-e753decda492"
version = "1.8.2+0"

[[deps.Xorg_libXinerama_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Xorg_libXext_jll"]
git-tree-sha1 = "a1a7eaf6c3b5b05cb903e35e8372049b107ac729"
uuid = "d1454406-59df-5ea1-beac-c340f2130bc3"
version = "1.1.5+0"

[[deps.Xorg_libXrandr_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Xorg_libXext_jll", "Xorg_libXrender_jll"]
git-tree-sha1 = "b6f664b7b2f6a39689d822a6300b14df4668f0f4"
uuid = "ec84b674-ba8e-5d96-8ba1-2a689ba10484"
version = "1.5.4+0"

[[deps.Xorg_libXrender_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Xorg_libX11_jll"]
git-tree-sha1 = "a490c6212a0e90d2d55111ac956f7c4fa9c277a6"
uuid = "ea2f1a96-1ddc-540d-b46f-429655e07cfa"
version = "0.9.11+1"

[[deps.Xorg_libpthread_stubs_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl"]
git-tree-sha1 = "c57201109a9e4c0585b208bb408bc41d205ac4e9"
uuid = "14d82f49-176c-5ed1-bb49-ad3f5cbd8c74"
version = "0.1.2+0"

[[deps.Xorg_libxcb_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "XSLT_jll", "Xorg_libXau_jll", "Xorg_libXdmcp_jll", "Xorg_libpthread_stubs_jll"]
git-tree-sha1 = "1a74296303b6524a0472a8cb12d3d87a78eb3612"
uuid = "c7cfdc94-dc32-55de-ac96-5a1b8d977c5b"
version = "1.17.0+3"

[[deps.Xorg_libxkbfile_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Xorg_libX11_jll"]
git-tree-sha1 = "dbc53e4cf7701c6c7047c51e17d6e64df55dca94"
uuid = "cc61e674-0454-545c-8b26-ed2c68acab7a"
version = "1.1.2+1"

[[deps.Xorg_xcb_util_cursor_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Xorg_xcb_util_image_jll", "Xorg_xcb_util_jll", "Xorg_xcb_util_renderutil_jll"]
git-tree-sha1 = "04341cb870f29dcd5e39055f895c39d016e18ccd"
uuid = "e920d4aa-a673-5f3a-b3d7-f755a4d47c43"
version = "0.1.4+0"

[[deps.Xorg_xcb_util_image_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Pkg", "Xorg_xcb_util_jll"]
git-tree-sha1 = "0fab0a40349ba1cba2c1da699243396ff8e94b97"
uuid = "12413925-8142-5f55-bb0e-6d7ca50bb09b"
version = "0.4.0+1"

[[deps.Xorg_xcb_util_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Pkg", "Xorg_libxcb_jll"]
git-tree-sha1 = "e7fd7b2881fa2eaa72717420894d3938177862d1"
uuid = "2def613f-5ad1-5310-b15b-b15d46f528f5"
version = "0.4.0+1"

[[deps.Xorg_xcb_util_keysyms_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Pkg", "Xorg_xcb_util_jll"]
git-tree-sha1 = "d1151e2c45a544f32441a567d1690e701ec89b00"
uuid = "975044d2-76e6-5fbe-bf08-97ce7c6574c7"
version = "0.4.0+1"

[[deps.Xorg_xcb_util_renderutil_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Pkg", "Xorg_xcb_util_jll"]
git-tree-sha1 = "dfd7a8f38d4613b6a575253b3174dd991ca6183e"
uuid = "0d47668e-0667-5a69-a72c-f761630bfb7e"
version = "0.3.9+1"

[[deps.Xorg_xcb_util_wm_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Pkg", "Xorg_xcb_util_jll"]
git-tree-sha1 = "e78d10aab01a4a154142c5006ed44fd9e8e31b67"
uuid = "c22f9ab0-d5fe-5066-847c-f4bb1cd4e361"
version = "0.4.1+1"

[[deps.Xorg_xkbcomp_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Xorg_libxkbfile_jll"]
git-tree-sha1 = "ab2221d309eda71020cdda67a973aa582aa85d69"
uuid = "35661453-b289-5fab-8a00-3d9160c6a3a4"
version = "1.4.6+1"

[[deps.Xorg_xkeyboard_config_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Xorg_xkbcomp_jll"]
git-tree-sha1 = "691634e5453ad362044e2ad653e79f3ee3bb98c3"
uuid = "33bec58e-1273-512f-9401-5d533626f822"
version = "2.39.0+0"

[[deps.Xorg_xtrans_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl"]
git-tree-sha1 = "6dba04dbfb72ae3ebe5418ba33d087ba8aa8cb00"
uuid = "c5fb5394-a638-5e4d-96e5-b29de1b5cf10"
version = "1.5.1+0"

[[deps.Zlib_jll]]
deps = ["Libdl"]
uuid = "83775a58-1f1d-513f-b197-d71354ab007a"
version = "1.2.13+1"

[[deps.Zstd_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl"]
git-tree-sha1 = "446b23e73536f84e8037f5dce465e92275f6a308"
uuid = "3161d3a3-bdf6-5164-811a-617609db77b4"
version = "1.5.7+1"

[[deps.eudev_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Pkg", "gperf_jll"]
git-tree-sha1 = "431b678a28ebb559d224c0b6b6d01afce87c51ba"
uuid = "35ca27e7-8b34-5b7f-bca9-bdc33f59eb06"
version = "3.2.9+0"

[[deps.fzf_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl"]
git-tree-sha1 = "6e50f145003024df4f5cb96c7fce79466741d601"
uuid = "214eeab7-80f7-51ab-84ad-2988db7cef09"
version = "0.56.3+0"

[[deps.gperf_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Pkg"]
git-tree-sha1 = "0ba42241cb6809f1a278d0bcb976e0483c3f1f2d"
uuid = "1a1c6b14-54f6-533d-8383-74cd7377aa70"
version = "3.1.1+1"

[[deps.libaom_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl"]
git-tree-sha1 = "522c1df09d05a71785765d19c9524661234738e9"
uuid = "a4ae2306-e953-59d6-aa16-d00cac43593b"
version = "3.11.0+0"

[[deps.libass_jll]]
deps = ["Artifacts", "Bzip2_jll", "FreeType2_jll", "FriBidi_jll", "HarfBuzz_jll", "JLLWrappers", "Libdl", "Zlib_jll"]
git-tree-sha1 = "e17c115d55c5fbb7e52ebedb427a0dca79d4484e"
uuid = "0ac62f75-1d6f-5e53-bd7c-93b484bb37c0"
version = "0.15.2+0"

[[deps.libblastrampoline_jll]]
deps = ["Artifacts", "Libdl"]
uuid = "8e850b90-86db-534c-a0d3-1478176c7d93"
version = "5.11.0+0"

[[deps.libdecor_jll]]
deps = ["Artifacts", "Dbus_jll", "JLLWrappers", "Libdl", "Libglvnd_jll", "Pango_jll", "Wayland_jll", "xkbcommon_jll"]
git-tree-sha1 = "9bf7903af251d2050b467f76bdbe57ce541f7f4f"
uuid = "1183f4f0-6f2a-5f1a-908b-139f9cdfea6f"
version = "0.2.2+0"

[[deps.libevdev_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Pkg"]
git-tree-sha1 = "141fe65dc3efabb0b1d5ba74e91f6ad26f84cc22"
uuid = "2db6ffa8-e38f-5e21-84af-90c45d0032cc"
version = "1.11.0+0"

[[deps.libfdk_aac_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl"]
git-tree-sha1 = "8a22cf860a7d27e4f3498a0fe0811a7957badb38"
uuid = "f638f0a6-7fb0-5443-88ba-1cc74229b280"
version = "2.0.3+0"

[[deps.libinput_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Pkg", "eudev_jll", "libevdev_jll", "mtdev_jll"]
git-tree-sha1 = "ad50e5b90f222cfe78aa3d5183a20a12de1322ce"
uuid = "36db933b-70db-51c0-b978-0f229ee0e533"
version = "1.18.0+0"

[[deps.libpng_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Zlib_jll"]
git-tree-sha1 = "068dfe202b0a05b8332f1e8e6b4080684b9c7700"
uuid = "b53b4c65-9356-5827-b1ea-8c7a1a84506f"
version = "1.6.47+0"

[[deps.libvorbis_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Ogg_jll", "Pkg"]
git-tree-sha1 = "490376214c4721cdaca654041f635213c6165cb3"
uuid = "f27f6e37-5d2b-51aa-960f-b287f2bc3b7a"
version = "1.3.7+2"

[[deps.mtdev_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Pkg"]
git-tree-sha1 = "814e154bdb7be91d78b6802843f76b6ece642f11"
uuid = "009596ad-96f7-51b1-9f1b-5ce2d5e8a71e"
version = "1.1.6+0"

[[deps.nghttp2_jll]]
deps = ["Artifacts", "Libdl"]
uuid = "8e850ede-7688-5339-a07c-302acd2aaf8d"
version = "1.59.0+0"

[[deps.oneTBB_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl"]
git-tree-sha1 = "d5a767a3bb77135a99e433afe0eb14cd7f6914c3"
uuid = "1317d2d5-d96f-522e-a858-c73665f53c3e"
version = "2022.0.0+0"

[[deps.p7zip_jll]]
deps = ["Artifacts", "Libdl"]
uuid = "3f19e933-33d8-53b3-aaab-bd5110c3b7a0"
version = "17.4.0+2"

[[deps.x264_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Pkg"]
git-tree-sha1 = "4fea590b89e6ec504593146bf8b988b2c00922b2"
uuid = "1270edf5-f2f9-52d2-97e9-ab00b5d0237a"
version = "2021.5.5+0"

[[deps.x265_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Pkg"]
git-tree-sha1 = "ee567a171cce03570d77ad3a43e90218e38937a9"
uuid = "dfaa095f-4041-5dcd-9319-2fabd8486b76"
version = "3.5.0+0"

[[deps.xkbcommon_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Pkg", "Wayland_jll", "Wayland_protocols_jll", "Xorg_libxcb_jll", "Xorg_xkeyboard_config_jll"]
git-tree-sha1 = "63406453ed9b33a0df95d570816d5366c92b7809"
uuid = "d8fb68d0-12a3-5cfd-a85a-d49703b185fd"
version = "1.4.1+2"
"""

# ╔═╡ Cell order:
# ╟─fa570bdd-3772-4750-980d-d75cf268ffcf
# ╠═2adecdf9-45d8-4c18-8227-fb0a554ea3ca
# ╠═5308c0dd-3d0a-44db-9f6b-71b9e9587dfe
# ╟─c7786892-73cf-4e23-bfe6-339feae6f4de
# ╟─ef64d57f-30e6-45dd-b598-65728d31b77b
# ╠═adc7fdd3-b339-4573-9717-34d7d4ac0324
# ╠═aa0202ef-8dea-4c3f-a017-fe367efca375
# ╟─62c1471f-5803-4b61-85fd-4a3bafde8210
# ╟─f41c5399-987a-4122-b283-76f488eaabb7
# ╠═e64e3e12-d797-4fd0-b2e6-c371b8aebf82
# ╠═520977d9-d16c-4dc1-83e2-6d6bd058662c
# ╠═1d5b75bc-9b23-473d-bc43-5d492c621d5d
# ╠═bc054f16-5c7a-4b8d-b9af-abe0e1965573
# ╟─e513f638-a083-4bfb-aa3e-6450d37001b5
# ╠═40a64ec4-cdaa-497d-9283-c3c1da062d58
# ╟─5fa06031-f734-42e7-95bf-fd0daf506687
# ╠═98e84ada-af82-4715-b894-6a9e1153ebb8
# ╠═264da090-b49b-4203-903c-a2fe81f165aa
# ╟─7e195849-db71-401b-8c3c-68c712135390
# ╠═3006e2d4-c8b9-48a3-9857-5ab15b59238e
# ╠═a0c223be-1c3e-4fc2-aa5b-e6b6e077eb40
# ╠═b2203672-3079-49ec-a7f4-e09804136b86
# ╟─8546b716-93fc-4372-81be-f72566f8ad9d
# ╠═277b39c7-d1a6-44dc-ab94-0df434f45ebc
# ╠═f34a6fa6-2d59-41ad-93fd-e431c52357c9
# ╠═860dc015-a6f8-44e8-81d4-3c3391cef7dd
# ╠═301d6aed-a9f6-4a3c-9a41-0a4f693e1355
# ╠═9998b3e0-a799-42a5-889a-91908d1268dd
# ╠═744044c4-0ca8-400c-b049-71e16ef052d9
# ╟─9fef423f-6f85-48ab-86fa-7687af6ce184
# ╠═110b514a-6666-48f6-ba52-4b188caf9ca3
# ╟─3079d163-b5b0-4ad8-aaeb-0c32fe721f21
# ╠═9a2dc360-058b-4ba9-a477-bad65e2d2dae
# ╠═1c89e4be-6cb6-4c0b-a3b4-b48e07617470
# ╟─68af513d-c457-49f8-ba7c-d6ca7c142975
# ╟─96f047dc-eb1c-4520-bad0-b4670fbafe57
# ╠═56a91a7a-1e7f-400d-b18b-4d35f66238e8
# ╠═cb9e519b-d5e3-440f-8d5b-bc337fe1788e
# ╠═dee1baa0-8614-465c-b232-afba4df9fe5f
# ╠═8195e9a2-e85f-4e15-aba9-1096add9b77c
# ╠═d61b5b53-0ab1-4fee-8ce1-1845c54e918e
# ╠═3a72df8a-79a0-4087-beff-f839a5ddc133
# ╠═9d9ad896-0388-4802-bb0e-bc7f62db127f
# ╠═29891ae5-f88d-4618-acb2-ecf70cb4ed21
# ╠═42f16fad-8b1f-4292-8834-d25cd1eaa3db
# ╟─8a8e28a6-0b5a-49cf-9035-97c9bb59214a
# ╠═beb1d70b-d5eb-4f0e-9c40-3c9abbcf63b6
# ╠═b40cf08b-81ef-4055-9cdc-e7ef647a4830
# ╠═2ccb150c-a71f-42eb-b228-a3f301c0fa93
# ╠═e315acb0-f0a0-4a2d-adc3-c510e0f46997
# ╟─b00ddd51-7a31-4809-9830-05e76e2ff0f3
# ╟─073d70ef-da1e-47dd-b0e5-a532b936c883
# ╠═c45ce64f-2ae1-4120-adad-24c1f843498c
# ╟─39c8e8f6-6852-4f3e-84b3-72e8c4e3db11
# ╟─00bf348b-9e12-4486-a772-3fe1c26234cd
# ╟─79d035b0-df28-4e7b-bba2-f484fd24e14c
# ╟─6a72863a-c5d6-46d3-b2ce-ffe420a49549
# ╟─b0df71f8-b3a3-477a-b4aa-5702491840e1
# ╟─0cbe5265-a2d2-45b6-bc8a-60173db020f0
# ╟─4d344c68-f99a-4df5-be6f-cdf4cff29731
# ╟─c2437329-a343-4909-af0a-55820fcce5b3
# ╟─00000000-0000-0000-0000-000000000001
# ╟─00000000-0000-0000-0000-000000000002
