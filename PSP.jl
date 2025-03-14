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
md"""
## Grasshopper
"For designers who are exploring new shapes using generative algorithms, Grasshopper® is a graphical algorithm editor tightly integrated with Rhino’s 3-D modeling tools. Unlike RhinoScript, Grasshopper requires no knowledge of programming or scripting, but still allows designers to build form generators from the simple to the awe-inspiring. " [2]

Grasshopper allows the simple creation of parametric models in Rhino-3D, without programming experience. This makes it possible to visually see the model being created in real time, a useful attribute when creating complex models.
![PS Hull created by Grasshopper]
(https://raw.githubusercontent.com/jpkleinTUD/Potential_flow_assignment/refs/heads/main/Images/Rhino_grasshopper.png)
	
### The model

For this project, a parametric model of the PS was made. The model features 3 primary parts. The input paramters, the creation of the Brep model, and the export to Julia. 

![](https://raw.githubusercontent.com/jpkleinTUD/Potential_flow_assignment/refs/heads/main/Images/grasshopper_complete.png)

The first method attempted was to save the model as a STEP file, and use the NURBS.jl package to load the geometry.  
"""


# ╔═╡ fc46cecf-68dc-4f10-aad8-2ad952133e02
begin
	Patches = load(joinpath(@__DIR__, "data", "Model_ps.stp"));
	plotPatches(Patches, plotControlPoints=false, resolution=0.25)
end

# ╔═╡ 6af1e9d6-07f6-4c40-a277-ab6421d669ae
md"""
As can be seen in the plot, this does not seem to work properly. Diving into the documentation of NURBS.jl reveals the issue.

![](https://raw.githubusercontent.com/jpkleinTUD/Potential_flow_assignment/refs/heads/main/Images/nurbs_docs.png)

As it turns out, the majority of the model is not one of these two surface types. This setback meant a new method had to be devised. 

### Meshing and Export to julia
To work around the issue of NURBS not working on this model, Grasshopper's LunchBox module was used to create a quad mesh of the model. This method meshing  had a couple of benefits. First of all, the algorithm used implemented adaptive meshing, ensuring smaller panel sizes in places with more curvature. It also allowed the user to quickly check the mesh visually for any strange or missing sections. The target number of panels could be specified and the algorithm would do its best to create that number of panels. Finally, because the vertices were exported, determining which panels are surface panels was easy to do later on.

This mesh was then deconstructed into vertices, faces and normals. The next section will explain how these were then used to construct panels. 

[image of mesh export chain]

The file format used was JSON, as this provided the flexability to store both parameters of the the model used as well as the lists of vertices, faces and normals. This was done using a python block with the following code:

```python
import datetime
import json

n_panels = len(faces)
timestamp = datetime.datetime.now().strftime("%m%d_%H-%M")
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
"""

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

# ╔═╡ 860dc015-a6f8-44e8-81d4-3c3391cef7dd
# ╠═╡ disabled = true
#=╠═╡
q, ps, A = solve_sources(panels;Fn=Fn1);
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

# ╔═╡ adc7fdd3-b339-4573-9717-34d7d4ac0324
# ╠═╡ disabled = true
#=╠═╡
using NeumannKelvin, JSON, StaticArrays, LinearAlgebra, Plots, PlotlyBase,PlotlyKaleido, PlutoUI
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

# ╔═╡ 00000000-0000-0000-0000-000000000001
PLUTO_PROJECT_TOML_CONTENTS = """
[deps]
FileIO = "5789e2e9-d7fb-5bc7-8068-2c6fae9b9549"
NURBS = "dde13934-061e-461b-aa91-2c0fad390a0d"
PlotlyBase = "a03496cd-edff-5a9b-9e67-9cda94a718b5"
PlotlyJS = "f0f68f2c-4968-5e81-91da-67840de0976a"
WebIO = "0f1e0344-ec1d-5b48-a673-e5cf874b6c29"

[compat]
FileIO = "~1.17.0"
NURBS = "~0.8.0"
PlotlyBase = "~0.8.20"
PlotlyJS = "~0.18.15"
WebIO = "~0.8.21"
"""

# ╔═╡ 00000000-0000-0000-0000-000000000002
PLUTO_MANIFEST_TOML_CONTENTS = """
# This file is machine-generated - editing it directly is not advised

julia_version = "1.11.3"
manifest_format = "2.0"
project_hash = "090b77616928128a4efdff9c3c08e24b23182f0e"

[[deps.ArgTools]]
uuid = "0dad84c5-d112-42e6-8d28-ef12dabb789f"
version = "1.1.2"

[[deps.Artifacts]]
uuid = "56f22d72-fd6d-98f1-02f0-08ddc0907c33"
version = "1.11.0"

[[deps.AssetRegistry]]
deps = ["Distributed", "JSON", "Pidfile", "SHA", "Test"]
git-tree-sha1 = "b25e88db7944f98789130d7b503276bc34bc098e"
uuid = "bf4720bc-e11a-5d0c-854e-bdca1663c893"
version = "0.1.0"

[[deps.Base64]]
uuid = "2a0f44e3-6c83-55bd-87e4-b1978d98bd5f"
version = "1.11.0"

[[deps.BitFlags]]
git-tree-sha1 = "0691e34b3bb8be9307330f88d1a3c3f25466c24d"
uuid = "d1d4a3ce-64b1-5f1a-9ba4-7e7e69966f35"
version = "0.1.9"

[[deps.Blink]]
deps = ["Base64", "Distributed", "HTTP", "JSExpr", "JSON", "Lazy", "Logging", "MacroTools", "Mustache", "Mux", "Pkg", "Reexport", "Sockets", "WebIO"]
git-tree-sha1 = "bc93511973d1f949d45b0ea17878e6cb0ad484a1"
uuid = "ad839575-38b3-5650-b840-f874b8c74a25"
version = "0.12.9"

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
git-tree-sha1 = "c7acce7a7e1078a20a285211dd73cd3941a871d6"
uuid = "3da002f7-5984-5a60-b8a6-cbb66c0b333f"
version = "0.12.0"
weakdeps = ["StyledStrings"]

    [deps.ColorTypes.extensions]
    StyledStringsExt = "StyledStrings"

[[deps.ColorVectorSpace]]
deps = ["ColorTypes", "FixedPointNumbers", "LinearAlgebra", "Requires", "Statistics", "TensorCore"]
git-tree-sha1 = "8b3b6f87ce8f65a2b4f857528fd8d70086cd72b1"
uuid = "c3611d14-8923-5661-9e6a-0046d554d3a4"
version = "0.11.0"

    [deps.ColorVectorSpace.extensions]
    SpecialFunctionsExt = "SpecialFunctions"

    [deps.ColorVectorSpace.weakdeps]
    SpecialFunctions = "276daf66-3868-5448-9aa4-cd146d93841b"

[[deps.Colors]]
deps = ["ColorTypes", "FixedPointNumbers", "Reexport"]
git-tree-sha1 = "64e15186f0aa277e174aa81798f7eb8598e0157e"
uuid = "5ae59095-9a9b-59fe-a467-6f913c188581"
version = "0.13.0"

[[deps.CompilerSupportLibraries_jll]]
deps = ["Artifacts", "Libdl"]
uuid = "e66e0078-7015-5450-92f7-15fbd957f2ae"
version = "1.1.1+0"

[[deps.ConcurrentUtilities]]
deps = ["Serialization", "Sockets"]
git-tree-sha1 = "d9d26935a0bcffc87d2613ce14c527c99fc543fd"
uuid = "f0e56b4a-5159-44fe-b623-3e5288b988bb"
version = "2.5.0"

[[deps.DataAPI]]
git-tree-sha1 = "abe83f3a2f1b857aac70ef8b269080af17764bbe"
uuid = "9a962f9c-6df0-11e9-0e5d-c546b8b5ee8a"
version = "1.16.0"

[[deps.DataValueInterfaces]]
git-tree-sha1 = "bfc1187b79289637fa0ef6d4436ebdfe6905cbd6"
uuid = "e2d170a0-9d28-54be-80f0-106bbe20a464"
version = "1.0.0"

[[deps.Dates]]
deps = ["Printf"]
uuid = "ade2ca70-3891-5945-98fb-dc099432e06a"
version = "1.11.0"

[[deps.DelimitedFiles]]
deps = ["Mmap"]
git-tree-sha1 = "9e2f36d3c96a820c678f2f1f1782582fcf685bae"
uuid = "8bb1440f-4735-579b-a4ab-409b98df4dab"
version = "1.9.1"

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

[[deps.ExceptionUnwrapping]]
deps = ["Test"]
git-tree-sha1 = "d36f682e590a83d63d1c7dbd287573764682d12a"
uuid = "460bff9d-24e4-43bc-9d9f-a8973cb893f4"
version = "0.1.11"

[[deps.FileIO]]
deps = ["Pkg", "Requires", "UUIDs"]
git-tree-sha1 = "b66970a70db13f45b7e57fbda1736e1cf72174ea"
uuid = "5789e2e9-d7fb-5bc7-8068-2c6fae9b9549"
version = "1.17.0"
weakdeps = ["HTTP"]

    [deps.FileIO.extensions]
    HTTPExt = "HTTP"

[[deps.FileWatching]]
uuid = "7b1f6079-737a-58dc-b8bc-7a2ca5c1b5ee"
version = "1.11.0"

[[deps.FillArrays]]
deps = ["LinearAlgebra"]
git-tree-sha1 = "6a70198746448456524cb442b8af316927ff3e1a"
uuid = "1a297f60-69ca-5386-bcde-b61e274b549b"
version = "1.13.0"

    [deps.FillArrays.extensions]
    FillArraysPDMatsExt = "PDMats"
    FillArraysSparseArraysExt = "SparseArrays"
    FillArraysStatisticsExt = "Statistics"

    [deps.FillArrays.weakdeps]
    PDMats = "90014a1f-27ba-587c-ab20-58faa44d9150"
    SparseArrays = "2f01184e-e22b-5df5-ae63-d93ebab69eaf"
    Statistics = "10745b16-79ce-11e8-11f9-7d13ad32a3b2"

[[deps.FixedPointNumbers]]
deps = ["Statistics"]
git-tree-sha1 = "05882d6995ae5c12bb5f36dd2ed3f61c98cbb172"
uuid = "53c48c17-4a7d-5ca2-90c5-79b7896eea93"
version = "0.8.5"

[[deps.FunctionalCollections]]
deps = ["Test"]
git-tree-sha1 = "04cb9cfaa6ba5311973994fe3496ddec19b6292a"
uuid = "de31a74c-ac4f-5751-b3fd-e18cd04993ca"
version = "0.5.0"

[[deps.HTTP]]
deps = ["Base64", "CodecZlib", "ConcurrentUtilities", "Dates", "ExceptionUnwrapping", "Logging", "LoggingExtras", "MbedTLS", "NetworkOptions", "OpenSSL", "PrecompileTools", "Random", "SimpleBufferStream", "Sockets", "URIs", "UUIDs"]
git-tree-sha1 = "c67b33b085f6e2faf8bf79a61962e7339a81129c"
uuid = "cd3eb016-35fb-5094-929b-558a96fad6f3"
version = "1.10.15"

[[deps.Hiccup]]
deps = ["MacroTools", "Test"]
git-tree-sha1 = "6187bb2d5fcbb2007c39e7ac53308b0d371124bd"
uuid = "9fb69e20-1954-56bb-a84f-559cc56a8ff7"
version = "0.2.2"

[[deps.InteractiveUtils]]
deps = ["Markdown"]
uuid = "b77e0a4c-d291-57a0-90e8-8db25a27a240"
version = "1.11.0"

[[deps.IteratorInterfaceExtensions]]
git-tree-sha1 = "a3f24677c21f5bbe9d2a714f95dcd58337fb2856"
uuid = "82899510-4779-5014-852e-03e436cf321d"
version = "1.0.0"

[[deps.JLLWrappers]]
deps = ["Artifacts", "Preferences"]
git-tree-sha1 = "a007feb38b422fbdab534406aeca1b86823cb4d6"
uuid = "692b3bcd-3c85-4b1f-b108-f13ce0eb3210"
version = "1.7.0"

[[deps.JSExpr]]
deps = ["JSON", "MacroTools", "Observables", "WebIO"]
git-tree-sha1 = "b413a73785b98474d8af24fd4c8a975e31df3658"
uuid = "97c1335a-c9c5-57fe-bc5d-ec35cebe8660"
version = "0.5.4"

[[deps.JSON]]
deps = ["Dates", "Mmap", "Parsers", "Unicode"]
git-tree-sha1 = "31e996f0a15c7b280ba9f76636b3ff9e2ae58c9a"
uuid = "682c06a0-de6a-54ab-a142-c8b1cf79cde6"
version = "0.21.4"

[[deps.Kaleido_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Pkg"]
git-tree-sha1 = "43032da5832754f58d14a91ffbe86d5f176acda9"
uuid = "f7e6163d-2fa5-5f23-b69c-1db539e41963"
version = "0.2.1+0"

[[deps.LaTeXStrings]]
git-tree-sha1 = "dda21b8cbd6a6c40d9d02a73230f9d70fed6918c"
uuid = "b964fa9f-0449-5b57-a5c2-d3ea65f4040f"
version = "1.4.0"

[[deps.Lazy]]
deps = ["MacroTools"]
git-tree-sha1 = "1370f8202dac30758f3c345f9909b97f53d87d3f"
uuid = "50d2b5c4-7a5e-59d5-8109-a42b560f39c0"
version = "0.15.1"

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

[[deps.Libiconv_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl"]
git-tree-sha1 = "be484f5c92fad0bd8acfef35fe017900b0b73809"
uuid = "94ce4f54-9a6c-5748-9c1c-f9c7231a4531"
version = "1.18.0+0"

[[deps.LightXML]]
deps = ["Libdl", "XML2_jll"]
git-tree-sha1 = "3a994404d3f6709610701c7dabfc03fed87a81f8"
uuid = "9c8b4983-aa76-5018-a973-4c85ecc9e179"
version = "0.9.1"

[[deps.LinearAlgebra]]
deps = ["Libdl", "OpenBLAS_jll", "libblastrampoline_jll"]
uuid = "37e2e46d-f89d-539d-b4ee-838fcccc9c8e"
version = "1.11.0"

[[deps.Logging]]
uuid = "56ddb016-857b-54e1-b83d-db4d58db5568"
version = "1.11.0"

[[deps.LoggingExtras]]
deps = ["Dates", "Logging"]
git-tree-sha1 = "f02b56007b064fbfddb4c9cd60161b6dd0f40df3"
uuid = "e6f89c97-d47a-5376-807f-9c37f3926c36"
version = "1.1.0"

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

[[deps.Mmap]]
uuid = "a63ad114-7e13-5084-954f-fe012c677804"
version = "1.11.0"

[[deps.MozillaCACerts_jll]]
uuid = "14a3606d-f60d-562e-9121-12d972cd8159"
version = "2023.12.12"

[[deps.Mustache]]
deps = ["Printf", "Tables"]
git-tree-sha1 = "3b2db451a872b20519ebb0cec759d3d81a1c6bcb"
uuid = "ffc61752-8dc7-55ee-8c37-f3e9cdd09e70"
version = "1.0.20"

[[deps.Mux]]
deps = ["AssetRegistry", "Base64", "HTTP", "Hiccup", "MbedTLS", "Pkg", "Sockets"]
git-tree-sha1 = "7295d849103ac4fcbe3b2e439f229c5cc77b9b69"
uuid = "a975b10e-0019-58db-a62f-e48ff68538c9"
version = "1.0.2"

[[deps.NURBS]]
deps = ["FileIO", "LinearAlgebra", "StaticArrays", "Statistics", "Suppressor", "UUIDs", "WriteVTK"]
git-tree-sha1 = "bf71419f679856e52b2438b0118076159ae9198d"
uuid = "dde13934-061e-461b-aa91-2c0fad390a0d"
version = "0.8.0"
weakdeps = ["PlotlyJS"]

    [deps.NURBS.extensions]
    NURBSext = "PlotlyJS"

[[deps.NetworkOptions]]
uuid = "ca575930-c2e3-43a9-ace4-1e988b2c1908"
version = "1.2.0"

[[deps.Observables]]
git-tree-sha1 = "7438a59546cf62428fc9d1bc94729146d37a7225"
uuid = "510215fc-4207-5dde-b226-833fc4488ee2"
version = "0.5.5"

[[deps.OpenBLAS_jll]]
deps = ["Artifacts", "CompilerSupportLibraries_jll", "Libdl"]
uuid = "4536629a-c528-5b80-bd46-f80d51c5b363"
version = "0.3.27+1"

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

[[deps.OrderedCollections]]
git-tree-sha1 = "cc4054e898b852042d7b503313f7ad03de99c3dd"
uuid = "bac558e1-5e72-5ebc-8fee-abe8a469f55d"
version = "1.8.0"

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

[[deps.Pidfile]]
deps = ["FileWatching", "Test"]
git-tree-sha1 = "2d8aaf8ee10df53d0dfb9b8ee44ae7c04ced2b03"
uuid = "fa939f87-e72e-5be4-a000-7fc836dbe307"
version = "1.3.0"

[[deps.Pkg]]
deps = ["Artifacts", "Dates", "Downloads", "FileWatching", "LibGit2", "Libdl", "Logging", "Markdown", "Printf", "Random", "SHA", "TOML", "Tar", "UUIDs", "p7zip_jll"]
uuid = "44cfe95a-1eb2-52ea-b672-e2afdf69b78f"
version = "1.11.0"
weakdeps = ["REPL"]

    [deps.Pkg.extensions]
    REPLExt = "REPL"

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

[[deps.PlotlyJS]]
deps = ["Base64", "Blink", "DelimitedFiles", "JSExpr", "JSON", "Kaleido_jll", "Markdown", "Pkg", "PlotlyBase", "PlotlyKaleido", "REPL", "Reexport", "Requires", "WebIO"]
git-tree-sha1 = "e415b25fdec06e57590a7d5ac8e0cf662fa317e2"
uuid = "f0f68f2c-4968-5e81-91da-67840de0976a"
version = "0.18.15"

    [deps.PlotlyJS.extensions]
    CSVExt = "CSV"
    DataFramesExt = ["DataFrames", "CSV"]
    IJuliaExt = "IJulia"
    JSON3Ext = "JSON3"

    [deps.PlotlyJS.weakdeps]
    CSV = "336ed68f-0bac-5ca0-87d4-7b16caf5d00b"
    DataFrames = "a93c6f00-e57d-5684-b7b6-d8193f3e46c0"
    IJulia = "7073ff75-c697-5162-941a-fcdaad2a7d2a"
    JSON3 = "0f8b85d8-7281-11e9-16c2-39a750bddbf1"

[[deps.PlotlyKaleido]]
deps = ["Artifacts", "Base64", "JSON", "Kaleido_jll"]
git-tree-sha1 = "ba551e47d7eac212864fdfea3bd07f30202b4a5b"
uuid = "f2990250-8cf9-495f-b13a-cce12b45703c"
version = "2.2.6"

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

[[deps.Printf]]
deps = ["Unicode"]
uuid = "de0858da-6303-5e67-8744-51eddeeeb8d7"
version = "1.11.0"

[[deps.REPL]]
deps = ["InteractiveUtils", "Markdown", "Sockets", "StyledStrings", "Unicode"]
uuid = "3fa0cd96-eef1-5676-8a61-b3b8758bbffb"
version = "1.11.0"

[[deps.Random]]
deps = ["SHA"]
uuid = "9a3f8284-a2c9-5f02-9a11-845980a1fd5c"
version = "1.11.0"

[[deps.Reexport]]
git-tree-sha1 = "45e428421666073eab6f2da5c9d310d99bb12f9b"
uuid = "189a3867-3050-52da-a836-e630ba90ab69"
version = "1.2.2"

[[deps.Requires]]
deps = ["UUIDs"]
git-tree-sha1 = "62389eeff14780bfe55195b7204c0d8738436d64"
uuid = "ae029012-a4dd-5104-9daa-d747884805df"
version = "1.3.1"

[[deps.SHA]]
uuid = "ea8e919c-243c-51af-8825-aaa63cd721ce"
version = "0.7.0"

[[deps.Serialization]]
uuid = "9e88b42a-f829-5b0c-bbe9-9e923198166b"
version = "1.11.0"

[[deps.SimpleBufferStream]]
git-tree-sha1 = "f305871d2f381d21527c770d4788c06c097c9bc1"
uuid = "777ac1f9-54b0-4bf8-805c-2214025038e7"
version = "1.2.0"

[[deps.Sockets]]
uuid = "6462fe0b-24de-5631-8697-dd941f90decc"
version = "1.11.0"

[[deps.StaticArrays]]
deps = ["LinearAlgebra", "PrecompileTools", "Random", "StaticArraysCore"]
git-tree-sha1 = "0feb6b9031bd5c51f9072393eb5ab3efd31bf9e4"
uuid = "90137ffa-7385-5640-81b9-e52037218182"
version = "1.9.13"

    [deps.StaticArrays.extensions]
    StaticArraysChainRulesCoreExt = "ChainRulesCore"
    StaticArraysStatisticsExt = "Statistics"

    [deps.StaticArrays.weakdeps]
    ChainRulesCore = "d360d2e6-b24c-11e9-a2a3-2a2ae2dbcce4"
    Statistics = "10745b16-79ce-11e8-11f9-7d13ad32a3b2"

[[deps.StaticArraysCore]]
git-tree-sha1 = "192954ef1208c7019899fbf8049e717f92959682"
uuid = "1e83bf80-4336-4d27-bf5d-d5a4f845583c"
version = "1.4.3"

[[deps.Statistics]]
deps = ["LinearAlgebra"]
git-tree-sha1 = "ae3bb1eb3bba077cd276bc5cfc337cc65c3075c0"
uuid = "10745b16-79ce-11e8-11f9-7d13ad32a3b2"
version = "1.11.1"

    [deps.Statistics.extensions]
    SparseArraysExt = ["SparseArrays"]

    [deps.Statistics.weakdeps]
    SparseArrays = "2f01184e-e22b-5df5-ae63-d93ebab69eaf"

[[deps.StyledStrings]]
uuid = "f489334b-da3d-4c2e-b8f0-e476e12c162b"
version = "1.11.0"

[[deps.Suppressor]]
deps = ["Logging"]
git-tree-sha1 = "6dbb5b635c5437c68c28c2ac9e39b87138f37c0a"
uuid = "fd094767-a336-5f1f-9728-57cf17d0bbfb"
version = "0.2.8"

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

[[deps.TranscodingStreams]]
git-tree-sha1 = "0c45878dcfdcfa8480052b6ab162cdd138781742"
uuid = "3bb67fe8-82b1-5028-8e26-92a6c54297fa"
version = "0.11.3"

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

[[deps.VTKBase]]
git-tree-sha1 = "c2d0db3ef09f1942d08ea455a9e252594be5f3b6"
uuid = "4004b06d-e244-455f-a6ce-a5f9919cc534"
version = "1.0.1"

[[deps.WebIO]]
deps = ["AssetRegistry", "Base64", "Distributed", "FunctionalCollections", "JSON", "Logging", "Observables", "Pkg", "Random", "Requires", "Sockets", "UUIDs", "WebSockets", "Widgets"]
git-tree-sha1 = "0eef0765186f7452e52236fa42ca8c9b3c11c6e3"
uuid = "0f1e0344-ec1d-5b48-a673-e5cf874b6c29"
version = "0.8.21"

[[deps.WebSockets]]
deps = ["Base64", "Dates", "HTTP", "Logging", "Sockets"]
git-tree-sha1 = "4162e95e05e79922e44b9952ccbc262832e4ad07"
uuid = "104b5d7c-a370-577a-8038-80a2059c5097"
version = "1.6.0"

[[deps.Widgets]]
deps = ["Colors", "Dates", "Observables", "OrderedCollections"]
git-tree-sha1 = "e9aeb174f95385de31e70bd15fa066a505ea82b9"
uuid = "cc8bc4a8-27d6-5769-a93b-9d913e69aa62"
version = "0.6.7"

[[deps.WriteVTK]]
deps = ["Base64", "CodecZlib", "FillArrays", "LightXML", "TranscodingStreams", "VTKBase"]
git-tree-sha1 = "1d8042d58334ab7947ce505709df7009da6f3375"
uuid = "64499a7a-5c06-52f2-abe2-ccb03c286192"
version = "1.21.1"

[[deps.XML2_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Libiconv_jll", "Zlib_jll"]
git-tree-sha1 = "b8b243e47228b4a3877f1dd6aee0c5d56db7fcf4"
uuid = "02c8fc9c-b97f-50b9-bbe4-9be30ff0a78a"
version = "2.13.6+1"

[[deps.Zlib_jll]]
deps = ["Libdl"]
uuid = "83775a58-1f1d-513f-b197-d71354ab007a"
version = "1.2.13+1"

[[deps.libblastrampoline_jll]]
deps = ["Artifacts", "Libdl"]
uuid = "8e850b90-86db-534c-a0d3-1478176c7d93"
version = "5.11.0+0"

[[deps.nghttp2_jll]]
deps = ["Artifacts", "Libdl"]
uuid = "8e850ede-7688-5339-a07c-302acd2aaf8d"
version = "1.59.0+0"

[[deps.p7zip_jll]]
deps = ["Artifacts", "Libdl"]
uuid = "3f19e933-33d8-53b3-aaab-bd5110c3b7a0"
version = "17.4.0+2"
"""

# ╔═╡ Cell order:
# ╟─fa570bdd-3772-4750-980d-d75cf268ffcf
# ╠═2adecdf9-45d8-4c18-8227-fb0a554ea3ca
# ╟─5308c0dd-3d0a-44db-9f6b-71b9e9587dfe
# ╠═fc46cecf-68dc-4f10-aad8-2ad952133e02
# ╠═6af1e9d6-07f6-4c40-a277-ab6421d669ae
# ╠═c7786892-73cf-4e23-bfe6-339feae6f4de
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
