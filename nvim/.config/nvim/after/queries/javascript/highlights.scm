;; extends

(("function" @keyword) (#set! conceal "ƒ"))

((arrow_function 
  parameters: (formal_parameters 
                "(" @open)
) (#set! conceal "λ"))

((arrow_function 
  parameters: (formal_parameters 
                ")" @close)
) (#set! conceal ""))
