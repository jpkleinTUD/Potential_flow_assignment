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

# ╔═╡ c7786892-73cf-4e23-bfe6-339feae6f4de


# ╔═╡ c2437329-a343-4909-af0a-55820fcce5b3
begin
	@eval Main.PlutoRunner format_output(x::Float64; context = default_iocontext) = format_output_default(round(x; digits = 3), context)
	@eval Main.PlutoRunner format_output(x::AbstractArray{Float64}; context = default_iocontext) = format_output_default(round.(x; digits = 3), context)
end;

# ╔═╡ Cell order:
# ╠═fa570bdd-3772-4750-980d-d75cf268ffcf
# ╠═c7786892-73cf-4e23-bfe6-339feae6f4de
# ╟─c2437329-a343-4909-af0a-55820fcce5b3
