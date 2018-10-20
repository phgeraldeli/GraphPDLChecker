#lang racket


(struct graph-body (W R V)) ; Estrutura do grafo será vertices relações valoração
(define all-graph ( graph-body (list 'A 'B) #| Vertices |# (list (list 'alfa(list 'a 'b) 'alfa (list 'b 'a)) 'beta (list 'a 'b))#| Relações alfa e beta |# (list (list 'psi 'omega) (list 'psi)) #|  Valorações em A e B|#))
  
(struct PDL (program))
(define allprog ( PDL (list '[alfa]'psi'[alfa]'psi?'omega)))



#| Duvidas:
    Estrutura do grafo precisa da valoração?
    Estrutura do PDL será [alfa]'psi ou só alfa;psi?;beta
    Como checar se existe uma arestra dentro da lista
    input do arquivo para dentro do struct
    Como utilizar o charactere especial ; sem virar comentário
|#