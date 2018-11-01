#lang racket

;(require graph)

(struct graph-body (W R)); Estrutura do grafo será vertices(Nome, "visitado") e relações (transição, destino)

(define grafo1 ( graph-body (list '(A 0) '(B 0) '(C 0)) #| Vertices |#
                            (list '((alfa B)) '((alfa B) (beta C)) '())))
                            #| (relacoes de A)      (relacoes de B)                (relacoes de C)|#
#|vertices é uma lista de listas, sendo cada lista uma um vertice e seu estado de visitado|#
#|relacoes é uma lista de listas de listas, sendo cada lista as relacoes de um vertice, e a relacao uma lista com "transição, destino" |#

(struct PDL (program))
(define allprog ( PDL (list 'alfa 'beta 'alfa)))

(define (vertices grafo)
  (graph-body-W grafo))

(define (relacoes grafo)
  (graph-body-R grafo))

(define (mostrarvertices grafo)
  (println "Vertices do grafo")
  (mostrarverticesrec (vertices grafo)))

(define (mostrarverticesrec vertices)
  (write (car (car vertices)))
  (if (equal? '() (cdr vertices))
      (values)
      (write '-)
  )
  (if (equal? '() (cdr vertices))
      (values)
      (mostrarverticesrec (cdr vertices))
  )
)
   
;(define (checarVertice vertice relacoes programa))

(define (mostrarrelacoes grafo)
  (println "Relações do grafo")
  (mostrarrelacoesrec (vertices grafo) (relacoes grafo)))

(define (mostrarrelacoesrec vertices relacoes)
  (display "\n")
  (write (car (car vertices)))
  (display ":=")
  (if (equal? '() (car relacoes))
      (values)
      (mostrarrelacoesdeumvertice (car relacoes)))
  (if (equal? '() (cdr vertices))
      (values)
      (mostrarrelacoesrec (cdr vertices) (cdr relacoes))))
  
(define (mostrarrelacoesdeumvertice relvertice)
  ;(write relvertice)
  (write (car (car relvertice)))
  (write "->")
  (write (car (cdr (car relvertice))))
  (display " / ")
  (if (equal? '() (cdr relvertice))
      (values)
      (mostrarrelacoesdeumvertice (cdr relvertice))))
; Retorna #t se o grafo g possui o vertice v
(define (tem-vertice? g v) ;função auxiliar para diminuir o tamanho da chamada
  (verticerec? (vertices g) v))
(define (verticerec? gv vp)  ;chamada (verticered (vertices g) 'VERTICE)
  (if (equal? '() gv)
      #f
      (if (equal? vp (car (car gv)))
          #t
          (verticerec? (cdr gv) vp))))

(define (index-vertice g vp)
  (index-vertice-rec (vertices g) vp))
(define (index-vertice-rec gv vp)
  (if(equal? '() gv)
     #f
     (if (equal? vp (car (car gv)))
          (+ 0)
          (+ 1 (index-vertice-rec (cdr gv) vp)))))

  
; Retorna #t se o grafo g possui uma aresta do vertice o para o vertice d 
(define (tem-aresta? g o d)
  (tem-vertice? g o)(tem-vertice? g d)
  (for([i (vertices g)])
      (equal? o i)
        (for ([j (list-ref(relacoes g) (index-of (vertices g) i))])
          (equal? (car j) i) #t)))

(define (tem-aresta-rec? g o d)
  (if(tem-vertice? g o)
     (if(tem-vertice? g d)
        (tem-aresta-vertice? (list-ref (relacoes grafo1) (index-vertice grafo1 o)) d)
     #f)
   #f)
)

(define (tem-aresta-vertice? rel dest)
  (if (equal? rel '())
      #f
      (if(equal? (car (cdr (car rel))) dest)
         #t
         (tem-aresta-vertice? (cdr rel) dest))))
  


; Retorna #t se o grafo g possui uma aresta do vertice o para o vertice d marcada pela relação p
(define (tem-arestamarcada? g o d p)
  (tem-vertice? g o)(tem-vertice? g d)
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