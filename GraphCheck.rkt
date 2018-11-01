#lang racket
(struct graph-body (W R)); Estrutura do grafo será vertices(Nome, "visitado") e relações (transição, destino)
                            #| (relacoes de A)      (relacoes de B)                (relacoes de C)|#
#|vertices é uma lista de listas, sendo cada lista uma um vertice e seu estado de visitado|#
#|relacoes é uma lista de listas de listas, sendo cada lista as relacoes de um vertice,
 e a relacao uma lista com "transição, destino" |#
(define (vertices grafo)
  (graph-body-W grafo))

(define (relacoes grafo)
  (graph-body-R grafo))

(define (mostrarvertices grafo)
  (println "Vertices do grafo")
  (mostrarverticesrec (vertices grafo)))

(define (mostrarverticesrec vertices) ;printa vertices recursivamente
  (write (car vertices))
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

;printa relacoes de um vertice

(define (mostrarrelacoesrec vertices relacoes)
  (display "\n")
  (write (car vertices))
  (display ":=")
  (if (equal? '() (car relacoes))
      (values)
      (mostrarrelacoesdeumvertice (car relacoes)))
  (if (equal? '() (cdr vertices))
      (values)
      (mostrarrelacoesrec (cdr vertices) (cdr relacoes))))
  
(define (mostrarrelacoesdeumvertice relvertice) ;auxiliar da função acima
  ;(write relvertice)
  (write (car relvertice))
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
      (if (equal? vp (car gv))
          #t
          (verticerec? (cdr gv) vp))))

(define (index-vertice g vp)         ;retorna o indice do vertice procurado, e -inf caso nao exista
  (index-vertice-rec (vertices g) vp))
(define (index-vertice-rec gv vp)
  (if(equal? '() gv)
     -inf.f
     (if (equal? vp (car gv))
          0
          (+ 1 (index-vertice-rec (cdr gv) vp)))))

  
; Retorna #t se o grafo g possui uma aresta do vertice o para o vertice d 
(define (tem-aresta-rec? g o d)
  (if(tem-vertice? g o)
     (if(tem-vertice? g d)
        (tem-aresta-vertice? ( list-ref (relacoes g) (index-vertice g o)) d)
     #f)
   #f)
)

(define (tem-aresta-vertice? rel dest) ;função auxiliar de tem-aresta-rec?
  (if (equal? rel '())
      #f
      (if(equal? (car (cdr (cdr (car rel)))) dest)
         #t
         (tem-aresta-vertice? (cdr rel) dest))))
  
(define (tem-aresta-marcada-rec? g o d p)
  (if(tem-vertice? g o)
     (if(tem-vertice? g d)
        (tem-aresta-marcada-vertice? (list-ref (relacoes grafo1) (index-vertice grafo1 o)) d p)
     #f)
   #f)
)

(define (tem-aresta-marcada-vertice? rel dest p) ;função auxiliar de tem-aresta-marcada-rec?
  (if (equal? rel '())
      #f
      (if(equal? (car (cdr (cdr (car rel)))) dest)
         (if (equal? (car (cdr (car rel))) p)
              #t
              (tem-aresta-vertice? (cdr rel) dest))
         (tem-aresta-vertice? (cdr rel) dest))
  )
)

;(struct graph-body (W R))
;(define (vertices grafo)
 ; (graph-body-W grafo))

(struct PDL (program))

(define (programapdl prog)
  (PDL-program prog))

(struct sequential (prog))
(define (seqprog seq)
  (sequential-prog seq))
(struct nondeterministic (progA progB))
(struct iteration (prog))
  
(define (validgraph program g)
  
  (if (sequential? (car (programapdl program1)))
      #t
      #f
      ))

(define nd1 (nondeterministic 'B 'C))
(define seq1 (sequential nd1))
(define seq2 (sequential 'B))
(define program1 (PDL (list seq1 seq2)))



;"main"
;(struct graph-body (W R))
(define grafo1 ( graph-body (list 'A 'B 'C) #| Vertices |#
                            (list
                             '((0 alfa B))
                             '((0 alfa B) (0 beta C))
                             '()
                             )
               )
)





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