(setq reseau
      '((oradia zerind 71) (zerind oradea 71) (oradea sibiu 151) (sibiu oradea 151) (arad zerind 75)
        (zerind arad 75)
        (arad sibiu 140) (sibiu arad 140)  (sibiu RIMNICUVILCEA 80) (RIMNICUVILCEA sibiu 80)   
        (sibiu fagaras 99)  (fagaras sibiu 99) 
        (timisoara arad 118) (arad timisoara 118)
        (lugoj timisoara 111) (timisioara lugoj 111)
        (mehadia lugoj 70) (lugoj mehadia 70)
        (dobreta mehadia 75) (mehadia dobreta 75)  (dobreta craiova 120) (craiova dobreta 120)
        (pitesti craiova 138) (craiova pitesti 138)
        (pitesti RIMNICUVILCEA 97) (RIMNICUVILCEA pitesti 97) (pitesti bucharest 101)
        (bucharest pitesti 101) (bucharest giurgiu 90) (giurgiu bucharest 90)
        (urziceni bucharest 85)
        (bucharest urziceni 85) (bucharest fagaras 211) (fagaras bucharest 211)
        (craiova rimnicuvilcea 146) (rimnicuvilcea craiova 146)
        (hirsova urziceni 98) (urziceni hirsova 98)  (hirsova eforie 86) (eforie hirsova 86)
        (vaslui urziceni 142) (urziceni vaslui 142) (lasi vaslui 92) (vaslui lasi 82) 
        (neamt lasi 87) (lasi neamt 87)
        ))


(defun heuristique(noeud)
  (cond
   ((equal noeud 'arad) 366)
   ((equal noeud 'dobreta) 242)
 ((equal noeud 'giurgiu) 77)
 ((equal noeud 'lugoj) 244)
 ((equal noeud 'oradea) 380)
 ((equal noeud 'sibiu) 253)
 ((equal noeud 'vaslui) 199)
 ((equal noeud 'BUCHAREST) 0)
 ((equal noeud 'hirsova) 151)
 ((equal noeud 'mehadia) 241)
 ((equal noeud 'pitesti) 100)
 ((equal noeud 'timisoara) 329)
 ((equal noeud 'zerind) 374)
 ((equal noeud 'craiova) 160)
 ((equal noeud 'fagaras) 176)
 ((equal noeud 'lasi) 226)
 ((equal noeud 'neamt) 234)
 ((equal noeud 'RIMNICUVILCEA) 193)
 ((equal noeud 'urziceni) 80)
 ((equal noeud 'eforie) 161))
  )


(defun successeur (noeud edges)
  (if (null edges)
      nil
      (if (equal (caar edges) noeud)
          (cons (car edges) (successeur noeud (cdr edges)))
          (successeur noeud (cdr edges)))))

(defun cout (edge)
  (+ (car (cdr (cdr edge))) (car (cdr (cdr (cdr edge))))))

(defun getTuple (noeud edges)
  (if (null edges)
      nil
      (if (equal (car (car edges)) noeud)
          (car edges)
          (getTuple noeud (cdr edges)))))

(defun inserer (edge edges)
  (if (null edges)
      (list edge)
      (if (< (cout edge) (cout (car edges)))
          (cons edge edges)
          (cons (car edges) (inserer edge (cdr edges))))))

(defun supprimer (noeud edges)
  (if (null edges)
      nil
      (if (equal noeud (car (car edges)))
          (supprimer noeud (cdr edges))
          (cons (car edges) (supprimer noeud (cdr edges))))))

(defun retourner_element (position lst)
  (cond ((= position 0) (car lst))
        (t (retourner_element (- position 1) (cdr lst)))))

(defun inserer_descendants (tuple successeur) 
  (if (null successeur)
      open
  (cond ((getTuple (retourner_element 1 (car successeur)) open) 
                     (cond ((<(+(retourner_element 2 tuple) (retourner_element 2 (car successeur))) (retourner_element 2 (getTuple (retourner_element 1 (car successeur)) open ))) 
 (progn                                                                                                                                                                       
 (setq open (inserer (list (retourner_element 1 (car successeur)) (retourner_element 0 tuple) (+(retourner_element 2 tuple) (retourner_element 2 (car successeur))) (heuristique (retourner_element 1 (car successeur))))  
                    (supprimer (retourner_element 1 (car successeur)) open) ))
  (inserer_descendants tuple (cdr successeur))
   ))(t open)))
        
        
        ((getTuple (retourner_element 1 (car successeur)) closed) 
        (cond ((<(+(retourner_element 2 tuple) (retourner_element 2 (car successeur))) (retourner_element 2 (getTuple (retourner_element 1 (car successeur)) closed ))) 
        (progn                                                                                                                                                                       
        (setq open (inserer (list (retourner_element 1 (car successeur)) (retourner_element 0 tuple) (+(retourner_element 2 tuple) (retourner_element 2 (car successeur))) (heuristique (retourner_element 1 (car successeur))))  
                           open ))
          (setq closed (supprimer (retourner_element 1 (car successeur)) closed))
          (inserer_descendants tuple (cdr successeur))
         ))(t open)))   
  (t 
    (progn 
       (setq open (inserer (list (retourner_element 1 (car successeur)) (retourner_element 0 tuple) (+(retourner_element 2 tuple) (retourner_element 2 (car successeur))) (heuristique (retourner_element 1 (car successeur)))) open))
       (inserer_descendants tuple (cdr successeur))               
     )
   )
   )))

(defun get_parent (noeud edges)
  (if (null edges)
      nil
      (if (equal noeud (car (car edges)))
          (cadr (car edges))
          (get_parent noeud (cdr edges)))))

(defun reverse-sol (noeud edges)
  (if (null noeud)
      '()
      (cons noeud (reverse-sol (get_parent noeud edges) edges))))

(defun cons-sol (noeud edges)
  (reverse (reverse-sol noeud edges)))

(defun astar ()
  (cond ((null open) nil)
        (t (let ((tuple (car open)))
             (setq closed (cons tuple closed))
             (setq open (cdr open))
             (cond ((progn (equal (car tuple) goal))
                    (cons-sol goal closed))
                   (t (setq open (inserer_descendants tuple (successeur (car tuple) reseau)))
                      (astar)))))))

(defun startAstar (start finish)
  (setq goal finish)
  (setq open (list (list start nil 0 (heuristique start))))
  (setq closed nil)
  (astar))

(startAstar 'arad 'BUCHAREST)
open
closed
