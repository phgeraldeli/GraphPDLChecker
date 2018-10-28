#lang racket


(struct graph-body (W R)) ; Estrutura do grafo será vertices relações valoração
(define grafo1 ( graph-body (list 'A 'B 'C) #| Vertices |# (list '(alfa B) (list '(alfa B) '(beta C)) '()))) #| Relações alfa e beta |#
 
(struct PDL (program))
(define allprog ( PDL (list 'alfa 'beta 'alfa)))

(define (vertices grafo)
  (graph-body-W grafo))

(define (relacoes grafo)
  (graph-body-R grafo))

(define (mostrarvertices grafo)
  (for ([i (vertices grafo)])
    (print i))) #| fazer alguma coisa com os vertices |#

(define (mostrarrelacoes grafo)
  (for ([i (vertices grafo)])
    (println i)
    (println (list-ref (relacoes grafo) (index-of (vertices grafo) i)))
    ))


#| Duvidas:
    Estrutura do grafo precisa da valoração? Não
    Estrutura do PDL será [alfa]'psi ou só alfa;psi?;beta? alfa;psi;beta
    Como checar se existe uma arestra dentro da lista? car e cdr
    input do arquivo para dentro do struct? 
    Como utilizar o charactere especial ; sem virar comentário? 
|#