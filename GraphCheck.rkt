#lang racket

;(require graph)

(struct graph-body (W R)) ; Estrutura do grafo será vertices relações valoração
(define grafo1 ( graph-body (list 'A 'B 'C) #| Vertices |# (list '('(alfa B)) '('(alfa B) '(beta C)) '('())))) #| (relacoes de A)(relacoes de B)(relacoes de C)|#
 
(struct PDL (program))
(define allprog ( PDL (list 'alfa 'beta 'alfa)))

(define (vertices grafo)
  (graph-body-W grafo))

(define (relacoes grafo)
  (graph-body-R grafo))

(define (mostrarvertices grafo)
  (for ([i (vertices grafo)])
    (print i))) #| fazer alguma coisa com os vertices |#

;(define (checarVertice vertice relacoes programa))

(define (mostrarrelacoes grafo)
  (for ([i (vertices grafo)])
    (println i)
    (println (list-ref (relacoes grafo) (index-of (vertices grafo) i)))))


; Retorna #t se o grafo g possui o vertice v
(define (tem-vertice g v)
  (for([i (vertices g)])
     (equal? v i)#t))


; Retorna #t se o grafo g possui uma aresta do vertice o para o vertice d 
(define (tem-aresta? g o d)
  (tem-vertice g o)(tem-vertice g d)
  (for([i (vertices g)])
      (equal? o i)
        (for ([j (list-ref(relacoes g) (index-of (vertices g) i))])
          (equal? (car j) i) #t)))


; Retorna #t se o grafo g possui uma aresta do vertice o para o vertice d marcada pela relação p
(define (tem-arestamarcada? g o d p)
  (tem-vertice g o)(tem-vertice g d)
     (for([i (vertices g)])
       (equal? o i) 
        (for ([j (list-ref(relacoes g) (index-of (vertices g) i))])
          (and (equal? (cdr j) i) (equal? (car j) p) #t))))




;percorre o PDL a partir da posicao atual retornando o bloco daquela posicao.

;(define (percorrerPDL program posAtual))

;Problema no equal, não está restornando true nas funções: tem-vertice tem-aresta tem-arestamarcada
#|
(define (run grafo program)
  (for ([i (vertices grafo)])
    (println i)
    (println (list-ref (relacoes grafo) (index-of (vertices grafo) i))
    (checarrelacoes i (list-ref (relacoes grafo) (index-of (vertices grafo) i) program))))
|#



;(define g (weighted-graph/directed '((alfa A B) (beta A B) (alfa B B) (beta B C))))
;(edge-weight g 'A 'B)