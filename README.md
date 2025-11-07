# Street Canyon Tool


This tool combines the Open Street Map lines string data representing
the road network and LiDAR data to determine the potential for a street
canyon.

The definition of a street canyon is defined in [ADMS documentation
(chapter
2)](https://www.cerc.co.uk/environmental-software/assets/data/doc_techspec/P28_02.pdf)
as:

<center>

User inputs $H_{avg_{i}}$ Average building height  
$H_{max_{i}}$ Maximum building height  
$H_{min_{i}}$ Minimum building height  
$g_{i}$ Width from road centreline to canyon wall  
$b_{i}$ Length of road with adjacent buildings

Street canyon characterisation  
$H = (H_{avg_{L}} + H_{avg_{R}})/2)$ Overall average canyon height (m)  
$H_{min} = min(H_{avg_{L}},H_{avg_{R}})$ Overall minimum canyon height
(m)  
$H_{min} = max(H_{avg_{L}},H_{avg_{R}})$ Overall maximum canyon height
(m)  
$H_{\Delta_{i}} = 2(H_{avg_{i}}-H_{min_{i}})$ Range of bui8lding heights
for each side (m)  
$g = g_{L}+g_{R}$ Total canyon width (m)  
$\alpha_{i} = 1-b_{i}/L_{R}$ Porosity for each side, where $L_{R}$ is
the road length  
$H/g$ Ratio between canyon height and width  
$\phi_{c}$ Angle of the canyon segment relative to north (degrees)
</center>
