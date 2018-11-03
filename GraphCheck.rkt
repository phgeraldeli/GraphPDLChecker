#lang racket
(struct graph-body (W R))
(require racket/match)
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

(define (indice-vertice g vp)         ;retorna o indice do vertice procurado, e -inf caso nao exista
  (indice-vertice-rec (vertices g) vp))
(define (indice-vertice-rec gv vp)
  (if(equal? '() gv)
     -inf.f
     (if (equal? vp (car gv))
          0
          (+ 1 (indice-vertice-rec (cdr gv) vp)))))

  
; Retorna #t se o grafo g possui uma aresta do vertice o para o vertice d 
(define (tem-aresta-rec? g o d)
  (if(tem-vertice? g o)
     (if(tem-vertice? g d)
        (tem-aresta-vertice? ( list-ref (relacoes g) (indice-vertice g o)) d)
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
        (tem-aresta-marcada-vertice? (list-ref (relacoes g) (indice-vertice g o)) d p)
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
(define (relacoes-vertice g v);pega relações de um vertice especifico
  (relacoes-vertice-rec g (vertices g) v))

(define (relacoes-vertice-rec grafo grafo-vertices vertice-procurado);auxiliar da função acima
  (list-ref (relacoes grafo) (indice-vertice-rec grafo-vertices vertice-procurado)))


(define (index-arestas grafo vertice-origem programa-aresta);diz os indices das arestas de um vertice que usam o programa-aresta ()
  (index-arestas-aux (relacoes-vertice grafo vertice-origem) programa-aresta 0))


(define (index-arestas-aux rel-vert programa-aresta n);auxiliar da função acima
  (if (equal? '() rel-vert)
      '()
      (if (equal? (car (cdr (car rel-vert))) programa-aresta)
          (cons n (index-arestas-aux (cdr rel-vert) programa-aresta (+ n 1)))
          (index-arestas-aux (cdr rel-vert) programa-aresta (+ n 1)))))

(define (destino-aresta grafo vertice indice-aresta) ;retorna vertice Destino quando saimos do grafo com aresta do indice
  (if (equal? '() (relacoes-vertice grafo vertice))
      #f
      (car (cdr (cdr (list-ref (relacoes-vertice grafo vertice) indice-aresta))))))

(define (remove-elemento lista indice) ;função para remoção de uma aresta utilizada, para poder colocar nova aresta pintada
  (if(equal? indice 0)
     (cdr lista)
     (cons (car lista) (remove-elemento (cdr lista) (- indice 1)))))


(define (grafo-mudado grafo vertice indice-aresta programa destino cor)
  (graph-body (vertices grafo) (grafo-mudado-arestas-aux grafo (relacoes grafo) vertice (indice-vertice grafo vertice) indice-aresta programa destino cor)))

(define (grafo-mudado-arestas-aux grafo relacoes-grafo vertice indice-vertice indice-aresta programa destino cor)
  (if (equal? 0 indice-vertice)
      (cons (grafo-mudado-arestas-vertice-aux (relacoes-vertice grafo vertice) indice-aresta programa destino cor) (cdr relacoes-grafo))
      (cons (car relacoes-grafo) (grafo-mudado-arestas-aux grafo (cdr relacoes-grafo) vertice (- indice-vertice 1) indice-aresta programa destino cor))
      )
  )

(define (grafo-mudado-arestas-vertice-aux relacoes-vertice indice-aresta programa destino cor)
  (cons (list cor programa destino) (remove-elemento relacoes-vertice indice-aresta)))


(define (todas-arestas-percorridas? rel)
  (cond
    [(equal? '() rel) #t] 
    [(equal? (car rel) '()) (todas-arestas-percorridas? (cdr rel))]
    [(todas-arestas-perc-vertice-especifico? (car rel)) (todas-arestas-percorridas? (cdr rel))]
    [else #f]
    )
  )

(define (todas-arestas-perc-vertice-especifico? rel-vert)
  ;(println rel-vert)
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

(struct non-deterministic (lista))

(struct iteration (prog))
(struct atomic (prog))

(define (atom atomic)
  (atomic-prog atomic))


  
(define (valid-graph pdl g)
  (valid-graph-aux (car pdl) pdl g (car (vertices g))))


(define nd1 (non-deterministic (list 'B 'C)))
(define seq1 (sequential nd1))
(define seq2 (sequential 'B))
(define program1 (PDL (list seq1 seq2)))

(define atomicA (atomic 'A))
(define atomicB (atomic 'B))
;(define (index-arestas grafo vertice-origem programa-aresta)
(define (valid-graph-aux passo-atual resto-do-program grafo vertice)
  (println "entrou")
  (cond
    [(equal? resto-do-program '()) (todas-arestas-perc-vertice-especifico? (relacoes-vertice grafo vertice))]
    [(todas-arestas-perc-vertice-especifico? (relacoes-vertice grafo vertice))  #f]
    [(match passo-atual
      [atomic? (atomic-aux (index-arestas grafo vertice (atom passo-atual)) resto-do-program grafo vertice)]
      [_ (println "F")])]
    [else #f]
    )
  )
;(define (grafo-mudado grafo vertice indice-aresta programa destino cor)
;(define (destino-aresta grafo vertice indice-aresta)
(define (atomic-aux lista-arestas programa grafo vertice)
  (println "ATOMIC AUX")
  (cond
    [equal? (cdr lista-arestas) '()
      (if (valid-graph-aux (car programa) programa (grafo-mudado grafo vertice (car lista-arestas) programa (destino-aresta grafo vertice (car lista-arestas)) 1) (destino-aresta grafo vertice (car lista-arestas)))
          #t
          #f
      )]
    [(valid-graph-aux (car programa) (cdr programa) (grafo-mudado grafo vertice (car lista-arestas) programa (destino-aresta grafo vertice (car lista-arestas)) 1))
          #t
          (atomic-aux (cdr lista-arestas) programa grafo vertice)
        ]
  )
 )
  

(define grafo2 (graph-body (list 'A 'B)
                           (list
                            '((0 alfa B))
                            )
                           )
  )
(define entrada (list (atomic 'alfa)))
(valid-graph entrada grafo2)
(define grafo1 ( graph-body (list 'A 'B 'C) #| Vertices |#
                            (list
                             '((1 alfa B))
                             '((1 alfa B) (0 beta C) (0 beta A))
                             '()
                             )
               )
)