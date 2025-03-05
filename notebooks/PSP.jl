### A Pluto.jl notebook ###
# v0.20.4

using Markdown
using InteractiveUtils

# ╔═╡ fa570bdd-3772-4750-980d-d75cf268ffcf
md"""

# Potential flow assignment - Pioneering Spirit slot variance study

This notebook will cover the variance study of the dimensions of the Pioneering Spirit's slot using potential flow. It has been constructed by Albert Aperghis () and Jasper Klein (5343569).

Ships come in all shapes and size's. Some are small, some are large and some are absolutely ridiculously large. Enter the Pioneering Spirit. With it's 1,000,000 m^3 maximum displacement, the vessel can be considered the largest vessel in the world based on displaced water. While this vessel boasts many remarkable features, one cant help but notice the massive missing chunch from the bow of the vessel; the slot. 

![](https://raw.githubusercontent.com/jpkleinTUD/Potential_flow_assignment/5e0a23cb8579561da9db78cd721f6f718f46b1e5/Images/PS_slot.png)



see image

"""

# ╔═╡ 5308c0dd-3d0a-44db-9f6b-71b9e9587dfe
md"""
## Research question
"""

# ╔═╡ c7786892-73cf-4e23-bfe6-339feae6f4de
md"""
## Methodology
"""

# ╔═╡ 713a2565-5da8-43b7-9139-279ffc130bb3
md"""
## Validation
intro

### Comparisson to analytical methods
In order to validate the methodlogy used to compute the wave height for varying dimensions, a comparrisson can be made to the analytical description of the wave height as reflected from a wall. The expectation is that for increasing slot width's, the wave height that is observed should converge to the this analytical description. 

The analytical description for a wave reflected from a wall can be constructed by a summation of the incoming wave, and the reflected wave. This results in the analytical expression for a standing wave:

ζ_a(t) = ζ_i(t) + ζ_r(t) = ζ_a cos(-k*x-ω*t) +  ζ_a cos(k*x-ω*t) = 2ζ_a cos(k*x)cos(ω*t)

For a wall located at x = 0, the expression becomes:

ζ_a(t) = 2ζ_a cos(ω*t)

The plot below shows the wave height for a wave refelcted by a wall in infitite waterdepth. Furthermore the plot shows the wave heights in the slot for increasing slot widths. 



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
# ╠═fa570bdd-3772-4750-980d-d75cf268ffcf
# ╠═5308c0dd-3d0a-44db-9f6b-71b9e9587dfe
# ╠═c7786892-73cf-4e23-bfe6-339feae6f4de
# ╟─713a2565-5da8-43b7-9139-279ffc130bb3
# ╠═b0df71f8-b3a3-477a-b4aa-5702491840e1
# ╠═4d344c68-f99a-4df5-be6f-cdf4cff29731
# ╟─c2437329-a343-4909-af0a-55820fcce5b3
