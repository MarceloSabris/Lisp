(defvar *db* nil)
(defvar *dbCompra* nil)
(defvar totalCompra 0)
(defun add-recordProduto (cd ) (push cd *db*))
(defun add-recordCompra (cd ) (push cd *dbCompra*))

(defun  make-produto (nome preco )
  (list :nome nome  :preco preco  ))

(defun  make-produtoCompra  (nome preco qtd total )
  (list :nome nome  :preco preco :qtd qtd :total total  ))


(defun listar-produtos ()
(setq contador  0 )
  (dolist        (cd (sort (copy-list *db*) #'string-lessp :key #'first))
        
       ( if (= contador 10 )
         (prompt-read "Precione qualquer tecla para continuar")
        )
      (format t "~{~a:~10t~a~%~}~%" cd) 
        (setq contador ( + contador 1) )
   ) )



(defun entrada-de-produto ()

  (make-produto
   (prompt-read "Nome")
   (or (parse-integer (prompt-read "preco") :junk-allowed t) 0)
   ))

(defun pesquisa ( nome )

  (dolist        (cd (remove-if-not
  #'(lambda (cd) getf (equal (getf cd :nome) nome  )) *db*))
    
      (format t  "~a~%" (getf cd :preco)) 
   )
  
)


 (defun add-produtos ()
  (loop (add-recordProduto (entrada-de-produto) )
      (if (not (y-or-n-p "Deseja cadastrar outro produto ? [y/n]: ")) (return))))
   
(defun prompt-read (prompt)
  (format *query-io* "~a: " prompt)
  (force-output *query-io*)
  (read-line *query-io*))   
   

(defun comprar()

(loop 
 (format t "~a~%" "***** Escolha um produto da lista abaixo **** ")
    (listar-produtos)
    (setq nome (prompt-read "Nome") )
     (setq produto nil )

    (dolist (cd *db*)
      (if (equal (getf cd :nome) nome  )
            (setq produto cd )   )
     )                 
     (if (equal produto nil)
          (comprar))
      (getf produto :preco )
     (setq qtd (or (parse-integer (prompt-read "Entre com a quantidade") :junk-allowed t) 0) )

   (add-recordCompra   (make-produtoCompra nome  (getf produto :preco )  qtd  (* qtd  (getf produto :preco ))   ))
     (setq totalCompra  ( + totalCompra (* qtd  (getf produto :preco ))))
   (if (not (y-or-n-p "Deseja realizar outra compra ? [y/n]: ")) (return)))

   (dolist (cd *dbCompra*) 
                  
       (format t "~{~a~10t~a~10t~}~%" cd)  
	 	
     
    )
    
  (format t "~a~%" "***** Total **** ")
  (format t "~a~%" totalCompra)
)
   
  
 

(defun  menu ()
   (format t "~a~%" "***** Digite as opcoesoes abaixo do menu para acessar as funcionalidades **** ")
   (format t "~a~%" "      ***** 1 - Cadastrar  produtos  **** ")
   (format t "~a~%" "      ***** 2 - Listar produtos  **** ")
   (format t "~a~%" "      ***** 3 -  Pesquisar preco  **** ")
   (format t "~a~%" "      ***** 4 - Comprar produtos    **** ") 
   (format t "~a~%" "      ***** 5 - Sair    **** ")
   (or (parse-integer (prompt-read "Opcao:") :junk-allowed t) 0)
)

(defun iniciar() 
 
  (if  (equal *db* nil)
   (add-recordProduto   (make-produto  "teste" 10)   )
   )  
   (format t "~a~%" "***** Apenas um produto foi cadastrado automaticamente , para mais produtos use a opcao 1 do menu  **** ")

(setq opc (menu))
   
   
   (cond ((= opc 1)
        (add-produtos)
       (iniciar)
      )
      ((= opc 2)
         (listar-produtos)
		  (iniciar)
      )
      ((= opc 3)
       (pesquisa 
            (prompt-read "Entre com o nome")          )
	   (iniciar)
      )
      ((= opc 4)
       (comprar)
	   (iniciar)
      )
	  ((= opc 5)
       (princ "Obrigado por usar nosso sistemas!")
	   
      )
) ;_ cond
   
)
