# Creates a diagram for transforming questionnaires into XLSForm-compatible format from different sources
# Author: Abel Gelman

library(DiagrammeR) 

xls_flow1 <- DiagrammeR(diagram = " 
graph LR 
subgraph Web based 
A(PDF) --> B(Excel)
end
B --> C
C(R)
C --> D(CSV)
D -- copy & paste --> E(XLSForm)
F(Word) --> C
G(Messy Excel) --> C
                       
")


xls_flow1


# Creates a diagram of the workflow used to transform data for XLS Forms presented in the webinar

xls_flow2 <- DiagrammeR(diagram = " 
graph LR 
subgraph Web based 
A(PDF) --> B(Excel)
end
B --> C
C(R)
C --> D(CSV)
D -- copy & paste --> E(XLSForm)


                       
")

xls_flow2



