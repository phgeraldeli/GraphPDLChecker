#lang racket
(struct graph-body (W R)#:transparent)
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
  (if (equal? relacoes-vertice '())
      '()
      (if (equal? indice-aresta 0)
          (cons (list 1 programa destino) (cdr relacoes-vertice))
          (cons (car relacoes-vertice) (grafo-mudado-arestas-vertice-aux (cdr relacoes-vertice) (- indice-aresta 1) programa destino cor)))))


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


(define (conta-arestas arestas)
  (if (equal? (cdr arestas) '())
      (conta-arestas-vertice (car arestas))
      (+ (conta-arestas-vertice (car arestas))(conta-arestas (cdr arestas)))
      )
  )
(define (conta-arestas-vertice arestas-vertice)
  (if (equal? arestas-vertice '())
      0
      (+ 1 (conta-arestas-vertice (cdr arestas-vertice)))))
;(struct graph-body (W R))
;(define (vertices grafo)
 ; (graph-body-W grafo))

(struct PDL (program))

(define (programa-pdl prog)
  (PDL-program prog))

(struct sequential (prog1 prog2)#:transparent)
(define (strutc-prog1 prog)
  (sequential-prog1 prog))
(define (strutc-prog2 prog)
  (sequential-prog2 prog))

(struct non-deterministic (prog1 prog2)#:transparent)
(define (nd-prog1 nd)
  (non-deterministic-prog1 nd))
(define (nd-prog2 nd)
  (non-deterministic-prog2 nd))

(struct iteration (prog)#:transparent)
(define (iter-prog prog)
  (iteration-prog prog))
(struct atomic (prog)#:transparent)

(define (atom atomic) 
  (atomic-prog atomic))


(define nd1 (non-deterministic (atomic 'B) (atomic 'C)))


(define atomicA (atomic 'A))
(define atomicB (atomic 'B))
;(define (index-arestas grafo vertice-origem programa-aresta)
(define (verifica-final grafoverts)
  ;(println (car (car grafoverts)))
  ;(println grafoverts)
  (if (equal? grafoverts '())
      #f
      (or (todas-arestas-percorridas? (relacoes (car (car grafoverts)))) (verifica-final (cdr grafoverts)))))

(define (valid-graph pdl g)
  (if (equal? pdl '())
      (if (equal? (relacoes g) '(()))
          #t
          #f)
      (erro-no-grafo? (prog-base pdl (list (list g (list (car (vertices g)))))))
      )
  )

(define (erro-no-grafo? grafoverts)
  (define valido (verifica-final grafoverts))
  (if (equal? valido #t)
      #t
      (printa-erro grafoverts)
      )
  )
(define (printa-erro grafoverts)
  (println "Erro nas arestas com coloracao 0 ")
  (println grafoverts) 
  )
  
;sequencia
#|(define (valid-graph-aux pdl lista-grafos-vertices)
  (match pdl
    [atomic? 
|#

(define (prog-base prog grafoverts)
  ;(println prog)
  (if (equal? grafoverts '())
      '()
      (if (atomic? prog)
          (printaerro (atomic-prog-graphs prog grafoverts) prog)
          (if (non-deterministic? prog)
              (printaerro (non-deterministic-graph (nd-prog1 prog) (nd-prog2 prog) grafoverts) prog)
              (if (sequential? prog)
                  (printaerro (sequential-graph (sequential-prog1 prog) (sequential-prog2 prog) grafoverts) prog)
                  (if (iteration? prog)
                      (printaerro (iterative (* 2 (* (conta-arestas(relacoes (car (car grafoverts)))) (conta-arestas (relacoes (car (car grafoverts)))))) prog grafoverts) prog)
                      #f))))))

    
(define (printaerro resultado programa)
  (if (equal? resultado '())
      (printaerro-aux programa)
      resultado))

(define (printaerro-aux programa)
  (println programa)
  '()
  )
(define (atomic-prog-graphs prog-atomico grafoverts)
  ;(println grafoverts)
  (if (equal? grafoverts '())
      '()
      (append (atomic-prog-verts prog-atomico (car (car grafoverts)) (car (cdr (car grafoverts))))
              (atomic-prog-graphs prog-atomico (cdr grafoverts))
      )
  )
  )

(define (atomic-prog-verts prog-atomico grafo verts)
  ;(define erro (atomic-prog-verts prog-atomico grafo (cdr verts)))
  (if (equal? verts '())
      '()
      (if (equal? #f (atomic-prog-exec grafo (index-arestas grafo (car verts) (atom prog-atomico)) (atom prog-atomico) (car verts)))
          '()
          (append (atomic-prog-exec grafo (index-arestas grafo (car verts) (atom prog-atomico)) (atom prog-atomico) (car verts))
                  (atomic-prog-verts prog-atomico grafo (cdr verts)))
      )
      )
)



(define (non-deterministic-graph nd1 nd2 grafoverts)
  (define exec1 (prog-base nd1 grafoverts))
  (define exec2 (prog-base nd2 grafoverts))
  (define NDMERG (list (merge (append exec1 exec2))))
  NDMERG
  )

(define (change-final-vertex grafoverts vert)
  (if (equal? grafoverts '())
      '()
      (cons (list (car (car grafoverts)) (list vert)) (change-final-vertex (cdr grafoverts) vert))
      )
  )

(define (sequential-graph prog1 prog2 grafoverts)
  (define exec1 (prog-base prog1 grafoverts))
  (define exec2 (prog-base prog2 exec1))
  exec2
  )

(define (falso-ou-lista algo)
  (if (equal? #f algo)
      '()
      algo
      ))
  
;(define (index-arestas grafo vertice-origem programa-aresta) 
 (define (atomic-prog-exec grafo lista-arestas prog-atomico vertice)
  (if (equal? lista-arestas '())
      #f
      (atomic-prog-exec-aux grafo lista-arestas prog-atomico vertice)
      
      )
   )

(define (atomic-prog-exec-aux grafo lista-arestas prog-atomico vertice)
  (if (equal? lista-arestas '()) ;(lista-arestas '()
            '()
            (cons(list (grafo-mudado grafo vertice (car lista-arestas) prog-atomico (destino-aresta grafo vertice (car lista-arestas)) 1)
                       (list (destino-aresta grafo vertice (car lista-arestas))))
                 (atomic-prog-exec-aux grafo (cdr lista-arestas) prog-atomico vertice))
            )
  )
           
(define (iterative n prog grafoverts)
  (if (or (equal? n 0) (equal? grafoverts '()))
      '()
      (iterative-aux prog grafoverts n)
  )
  )
(define (iterative-aux prog grafoverts n)
  (define iteracao (prog-base (iter-prog prog) grafoverts))
  ;(println iteracao)
  (if (or (equal? iteracao '()) (equal? #f iteracao))
      grafoverts
      (if(equal? (car iteracao) '())
         grafoverts
         (append grafoverts (iterative (- n 1) prog iteracao)))))
      

(define grafo2 (graph-body (list 'A 'B)
                           (list
                            '((0 alfa B))
                            '((0 alfa A))
                            )
                           )
  )

(define entrada (list (atomic 'alfa) (atomic 'alfa) (atomic 'alfa)))
;(valid-graph entrada grafo2)


(define teste (list (list (graph-body '(A B C) '(((0 alfa B)) ((1 alfa C) (0 beta C) (0 beta A)) ())) '(C F K))
                    (list (graph-body '(A B C) '(((1 alfa B)) ((0 alfa C) (0 beta C) (0 beta A)) ())) '(B E P))
                    (list (graph-body '(A B C) '(((1 alfa B)) ((2 alfa C) (3 beta C) (0 beta A)) ())) '(B G Q))
                    (list (graph-body '(A B C) '(((1 alfa B)) ((0 alfa C) (0 beta C) (6 beta A)) ())) '(A H U))
                    ))
(define (acha-falso-na-lista grafoverts)
  (if (equal? grafoverts '())
      #t
      (if (equal? #f (car grafoverts))
          #t
          #f)))
(define (merge grafo-verts)
  (if (acha-falso-na-lista grafo-verts)
      '()
      (cond
        [(equal? grafo-verts '()) '()]
        [(equal? (cdr grafo-verts) '()) (car grafo-verts)]
        [else (if (equal? grafo-verts '())
                  '()
                  (list (graph-body (vertices (car (car grafo-verts)))
                          (junta-cor (relacoes (car (car grafo-verts))) (relacoes (car (merge (cdr grafo-verts))))))
                          (remove-duplicates (append (car (cdr (car grafo-verts))) (car (cdr (merge (cdr grafo-verts))))))
              )
        )]
  )
))


; junta as cores dado duas relacoes
(define (junta-cor rel1 rel2)
  (if (equal? '() rel2)
      rel1
      (map (λ (x y) (if (equal? '() x)
                    '()
                    (map (λ (a b) (if (> (car a) (car b))
                         (list (car a) (car (cdr a)) (car (cdr (cdr a))))
                         (list (car b) (car (cdr b)) (car (cdr (cdr b)))) )) x y))) rel1 rel2)
      )
  )

(define rel1 '( ((0 alfa B)) ((1 alfa C) (0 beta C) (0 beta A)) ()))
(define rel2 '( ((1 alfa B)) ((0 alfa C) (1 beta C) (0 beta A)) ()))
