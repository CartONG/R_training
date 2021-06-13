library(DiagrammeR) 

xls_flow <- DiagrammeR(diagram = " 
graph LR 
subgraph Web based 
A(PDF) --> B(Excel)
end
B --> C(R)
C --> D(CSV)
D -- copy & paste --> E(XLS)
")


gr_dot
