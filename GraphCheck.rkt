#lang racket


(struct graph-body (W R)) ; Estrutura do grafo será vertices relações valoração
(define all-graph ( graph-body (list 'A 'B) #| Vertices |# (list (list '(A B alfa) '(B A alfa))))) #| Relações alfa e beta |#
  
(struct PDL (program))
(define allprog ( PDL (list alfa beta alfa)))



#| Duvidas:
    Estrutura do grafo precisa da valoração? Não
    Estrutura do PDL será [alfa]'psi ou só alfa;psi?;beta? alfa;psi;beta
    Como checar se existe uma arestra dentro da lista? car e cdr
    input do arquivo para dentro do struct? 
    Como utilizar o charactere especial ; sem virar comentário? 
|#