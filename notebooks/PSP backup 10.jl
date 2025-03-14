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

# ╔═╡ adc7fdd3-b339-4573-9717-34d7d4ac0324
# ╠═╡ disabled = true
#=╠═╡
using NeumannKelvin, JSON, StaticArrays, LinearAlgebra, Plots, PlotlyBase,PlotlyKaleido, PlutoUI
  ╠═╡ =#

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
