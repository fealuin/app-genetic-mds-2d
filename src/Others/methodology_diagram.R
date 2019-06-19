library(DiagrammeR)

# 0((Inicio))
# A[Población]
# B[Operaciones genéticas]
# D[Evaluación de fitness]
# E[Ordenamiento y selección]
# F{"¿Cond. de término?"}
# G[Población inicial]
# H((Fin))
# 0-->G;
# G-->A
# B-->D;
# D-->E;
# A-->F;
# E-->A;
# F-->|no|B;
# F-->|si|H;


# Create a simple NDF
nodes <-
  create_node_df(
    n=8,
    nodes = 1:8,
    type = "number",
    label = c('Inicio','Población inicial','Población','Operaciones genéticas','Evaluación de fitness','Ordenamiento y selección','¿Cond. de término?','Fin'),
    shape=c('circle','rectangle','rectangle','rectangle','rectangle','rectangle','diamond','rectangle')
    )

# Create a simple EDF
edges <-
  create_edge_df(
    from = c(1, 1, 3, 1),
    to = c(2, 3, 4, 4),
    rel = "related")

# Create the graph object,
# incorporating the NDF and
# the EDF, and, providing
# some global attributes
graph <-
  create_graph(
    nodes_df = nodes,
    edges_df = edges
    
    )
    #graph_attrs = "layout = neato",
    #node_attrs = "fontname = Helvetica",
    #edge_attrs = "color = gray20")

# View the graph
render_graph(graph)

DiagrammeR(diagram="
          graph TB;
          0((Inicio))
          A[Población];
          B[Operaciones genéticas];
          D[Evaluación de fitness];
          E[Ordenamiento y selección];
          F{ ¿Cond. de término?};
          G[Población inicial];
          H((Fin));
          0-->G;
          G-->A

          B-->D;
          D-->E;
          A-->F;
          E-->A;
          F-->|no|B;
          F-->|si|H;
        classDef default fill:#f9f,stroke:#333,stroke-width:4px;
           "
)

DiagrammeR(diagram="graph LR;
          
            1-->2
            2-->3
3-->4
4-->5
5-->6
6-->1
6-->2
6-->3
6-->4
6-->5

            
           "
)



DiagrammeR(diagram="graph LR;



subgraph OE1
  EM1
end

subgraph OE2
  EM2
end
subgraph OE3
  EM3.1
  EM3.2
  EM3.3
  EM3.4
  EM3.5
  EM3.6
EM3.7
           end
subgraph OE4
  EM4
end
subgraph OE5
  EM5
end



EM1-->EM2

EM2-->EM3.1
EM3.1-->EM3.2
EM3.2-->EM3.3
EM3.3-->EM3.4
EM3.4-->EM3.5
EM3.5-->EM3.6
EM3.6-->EM3.7
EM3.7-->EM4
EM3.6-->EM3.1
EM3.6-->EM3.2
EM3.6-->EM3.3
EM3.6-->EM3.4
EM3.6-->EM3.5
EM4-->EM5


                     "
)


Sys.setenv("LANGUAGE"="ES")
Sys.setenv(LANG = "es_CL.UTF-8")
DiagrammeR(diagram="
         gantt

title Plan de trabajo
  dateFormat %W         
    section Construcción AG
      diseño I y P: ag1, 2019-03-19, 3d
      diseño P0: ag2, after ag1,3d
      diseño OG: ag3, after ag2, 10d
      det fitness: ag4, after ag3, 1w
      det calidad soluciones: ag5, after ag4, 1w
      integración: ag6, after ag5,2w
    section Mejora AG
      Aplicar mejoras: m1, after ag6,3w
    section Parametrización
      Selección método: p1, after m1, 1w
      Aplicar método: p2, after p1,1w
    section Documentación
      Documentación: d1,2019-03-19, 13w
           ",locale='CL'
)




DiagrammeR("graph LR;
  A(Rounded)-->B[Squared];
  B-->C{A Decision};
  C-->D[Square One];
  C-->E[Square Two];
  style A fill:#E5E25F;
  style B fill:#87AB51;
  style C fill:#3C8937;
  style D fill:#23772C;  
  style E fill:#B6E6E6;"
)

DiagrammeR()



grViz("
           digraph nicegraph {
             
             # a 'graph' statement
             graph [overlap = true, fontsize = 20]
             
             # several 'node' statements
             node [shape = box,
                   fontname = Helvetica,fontsize=20]
            A[label='Población']; 
            B[label='Operaciones genéticas']; 
            C[label='Evaluación de fitness']; 
            D[label='Ordenamiento y selección']; 
            
            F[label='Población inicial']
             
            node[shape=diamond, fontname = Helvetica,fontsize=20]
              E[label='¿Cond. de término?']; 

             node [shape = circle,
                   fixedsize = true,
                   width = 0.9] // sets as circles
             1[label='Inicio']; 2[label='Fin']
             
             # several 'edge' statements
            edge[fontname = Helvetica,fontsize=20]
              1->F 
              F->A
              A->E
              E->2[label='Sí']
              E->B[label='No']
              B->C
              C->D
              D->A
              
           }
           ")

grViz("
           digraph nicegraph {
      
      # a 'graph' statement
      graph [overlap = true, fontsize = 20]
      
      # several 'node' statements
      node [shape = box,
      fontname = Helvetica,fontsize=20]
      A[label='Población']; 
      B[label='Operaciones genéticas']; 
      C[label='Evaluación de fitness']; 
      D[label='Ordenamiento y selección']; 
      
      F[label='Población inicial']
      
      node[shape=diamond, fontname = Helvetica,fontsize=20]
      E[label='¿Cond. de término?']; 
      
      node [shape = circle,
      fixedsize = true,
      width = 0.9] // sets as circles
      1[label='Inicio']; 2[label='Fin']
      
      # several 'edge' statements
      edge[fontname = Helvetica,fontsize=20]
      1->F 
      F->A
      A->E
      E->2[label='Sí']
      E->B[label='No']
      B->C
      C->D
      D->A
      
      }
      ")
