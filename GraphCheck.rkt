#lang racket
(struct graph-body (W R))
; Estrutura do grafo será vertices(Nome, "visitado") e relações (transição, destino)
                            #| (relacoes de A)      (relacoes de B)                (relacoes de C)|#
#|vertices é uma lista de listas, sendo cada lista uma um vertice e seu estado de visitado|#
#|relacoes é uma lista de listas de listas, sendo cada lista as relacoes de um vertice,
 e a relacao uma lista com "transição, destino" |#
(define (vertices grafo)
  (graph-body-W grafo))

(define (relacoes grafo)
  (graph-body-R grafo))

(define (mostrar-vertices grafo)
  (println "Vertices do grafo")
  (mostrar-vertices-rec (vertices grafo)))

(define (mostrar-vertices-rec vertices) ;printa vertices recursivamente
  (write (car vertices))
  (if (equal? '() (cdr vertices))
      (values)
      (write '-)
  )
  (if (equal? '() (cdr vertices))
      (values)
      (mostrar-vertices-rec (cdr vertices))
  )
)
   
;(define (checar-vertice vertice relacoes programa))

(define (mostrar-relacoes grafo)
  (println "Relações do grafo")
  (mostrar-relacoes-rec (vertices grafo) (relacoes grafo)))

;printa relacoes de um vertice

(define (mostrar-relacoes-rec vertices relacoes)
  (display "\n")
  (write (car vertices))
  (display ":=")
  (if (equal? '() (car relacoes))
      (values)
      (mostrar-relacoes-de-um-vertice (car relacoes)))
  (if (equal? '() (cdr vertices))
      (values)
      (mostrar-relacoes-rec (cdr vertices) (cdr relacoes))))
  
(define (mostrar-relacoes-de-um-vertice rel-vertice) ;auxiliar da função acima
  ;(write relvertice)
  (write (car rel-vertice))
  (write "->")
  (write (car (cdr (car rel-vertice))))
  (display " / ")
  (if (equal? '() (cdr rel-vertice))
      (values)
      (mostrar-relacoes-de-um-vertice (cdr rel-vertice))))



; Retorna #t se o grafo g possui o vertice v
(define (tem-vertice? g v) ;função auxiliar para diminuir o tamanho da chamada
  (vertice-rec? (vertices g) v))
(define (vertice-rec? gv vp)  ;chamada (verticered (vertices g) 'VERTICE)
  (if (equal? '() gv)
      #f
      (if (equal? vp (car gv))
          #t
          (vertice-rec? (cdr gv) vp))))

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
        (tem-aresta-marcada-vertice? (list-ref (relacoes g) (index-vertice g o)) d p)
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
(define (relacoes-vertice g v)
  (relacoes-vertice-rec g (vertices g) v))

(define (relacoes-vertice-rec grafo grafo-vertices vertice-procurado)
  (list-ref (relacoes grafo) (index-vertice-rec grafo-vertices vertice-procurado)))


(define (todas-arestas-percorridas? rel)
  (cond
    [(equal? '() rel) #t] 
    [(equal? (car rel) '()) (todas-arestas-percorridas? (cdr rel))]
    [(todas-arestas-perc-vertice-especifico? (car rel)) (todas-arestas-percorridas? (cdr rel))]
    [else #f]
    )
  )

(define (todas-arestas-perc-vertice-especifico? rel-vert)
  (match rel-vert
    ['() #t]
    [_ (match (car (car rel-vert))
         [0 #f]
         [1 (todas-arestas-perc-vertice-especifico? (cdr rel-vert))]
         )]
    )
)

;(struct graph-body (W R))
;(define (vertices grafo)
 ; (graph-body-W grafo))

(struct PDL (program))

(define (programa-pdl prog)
  (PDL-program prog))

(struct sequential (prog))
(define (seqprog seq)
  (sequential-prog seq))
(struct non-deterministic (progA progB))
(struct iteration (prog))
  
(define (valid-graph pdl g)
  (valid-graph-aux (programa-pdl) g (car (vertices g))))
#|
(define (valid-graph-aux program grafo vertice)
  (cond
    [(equal? program '()) (cond
                            [])]))
|#


#|Ideia de resolução
(define (validgraph program g)
  (match program
    [(sequential? (car (programapdl program1)))   
    Anda no programa e anda no grafo, Chama recursivamente] ;Sequencia Normal
    [(nondeterministic? (car (programapdl program1))) 
    Pega o caminho da esquerda chama recursivo, volta no backtracking chama o da direita recursivo, 
    anda no grafo] ;Escolha nao deterministica
    [interação? Fazer algo] ;Caso da interação
    ['() (FUNCAO QUE TESTA SE TODAS AS ARESTAS FORAM PERCORRIDAS) #t] ; 
    caso que o grafo terminou e o programa também.
    ['() #f] ; caso do programa terminar e o grafo nao 
    [_ #f] ;caso de o programa nao ter terminado mas o grafo sim
    )
 )
|#

(define nd1 (non-deterministic 'B 'C))
(define seq1 (sequential nd1))
(define seq2 (sequential 'B))
(define program1 (PDL (list seq1 seq2)))



;"main"
;(struct graph-body (W R))
(define grafo1 ( graph-body (list 'A 'B 'C) #| Vertices |#
                            (list
                             '((1 alfa B))
                             '((1 alfa B) (0 beta C))
                             '()
                             )
               )
)