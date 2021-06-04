(*convention:                                                 *)
(*          a non existant arc has a capcity of 0 and a null cost *)
module type CONTAINER = sig
  type 'a t 
  val create : unit ->  'a t
  val pop  : 'a t-> 'a
  val push : 'a t-> 'a -> unit
  val is_empty : 'a t -> bool
  val peek : 'a t -> 'a 
end
module Stack : CONTAINER = struct
  type 'a t = {mutable cont : 'a list}
  let create ()  =  {cont = []}
  let pop  s = let temp  =  List.hd s.cont in s.cont <- List.tl s.cont; temp 
  let push s x =  s.cont <- x :: s.cont 
  let is_empty s = s.cont = [] 
  let peek s = List.hd s.cont


end


module type INT_DYNAMIC_ARRAY = sig
  type t 
  (*TRACE EST INUTILLLLEEEEE*)

  
  (**create n   creates an empty int dynamic array of length n . It is preferable to have n being within a precise ordre of magnitude of the final size of the array*)
  val empty : t
  val fill : int-> int-> t  (*fill n obj :t*)
  val create : int-> t
  
  
  (**number of element contained in the array*)
  val size : t -> int
  
  (** add an int to the array*) 
  val add:  t -> int -> unit
  
  (** see content of the ith cell. An array is 0-indexed*)
  val see: t -> int -> int  
  val set : t -> int -> int -> unit
  (* val iter : (int -> unit) -> t -> unit *)
end

module D:INT_DYNAMIC_ARRAY  = struct
  (**We use dynamically resizing int array references, which first cell (index 0) points to the index of the next element to be added*)
  type t = int array ref

  let empty :t  = ref [|1|]
  let fill n obj :t = 
     
    if n = 0 then 
      empty
    else
      let a = Array.make (n+1) obj in  
      a.(0) <- 1;
      ref a

  let create n :t = fill n 0
  let next a = !a.(0)
  let space a = Array.length !a
  
  let size a = next a - 1
  
  (**If the array is too small to have a new element added to it then it is copied in a 2 times larger one*)
  let add a x = 
    let n =  space a in 
    if next a = n then (
      let copy_a i = 
        if i<n then
          !a.(i)
        else 0 
      in
      let new_a  =  Array.init (2*n) copy_a in

      new_a.(next a) <- x;
      new_a.(0) <- next a +1;
      a := new_a)
    else(
      !a.(next a) <- x;
      !a.(0) <- next a +1)
  

  (** The indexation of a dynamic array starts at 0 (first available cell)*)
  let see (a:t) i = 
    !a.(i+1)

  let set (a:t) i x = 
    !a.(i+1)<-x
  (* let iter f a = 
    let n = Array.length !a  in 
    for i = 1 to n-1 do 
      f !a.(i)
    done *)

end



module type FLOW_GR = 
  sig
    type t 
    type vertex  = int(*of type int pr print, ou faire un foncteur*)
    type edge = vertex *vertex
    type capacity = Capa of int | Infty
    type vpath = vertex list
    type epath = edge list

    (**capcity operators*)
    
    exception Unvalid_Capacity_Operation
    exception Uncapacitated_arc of edge
    exception Not_residual_graph of t 

    val (++) : capacity -> capacity -> capacity
    val (--) : capacity -> capacity -> capacity
    val ( ** ) : capacity -> capacity -> capacity
    
    val (>>) : capacity -> capacity -> bool

    (*special values contructors*)
    val empty : t
    val upper_capacity :  int

    (**constructors*)

    (**Creates an n edged graph. The size is definite.*)
    val create : int -> t 
    val add_edge : t -> edge -> unit
    (*val delete_edge : t -> edge -> unit*)
    
    (*mutators*)
    val set_flow : t-> edge -> int -> unit
    val set_cost : t -> edge -> int -> unit
    val set_capacity : t-> edge -> capacity -> unit
    val set_supply : t -> vertex -> int -> unit

    

    (**iterators*)
    val iter_ver :  (vertex -> unit) ->  t -> unit
    val iter_edg : (edge -> unit) ->  t  -> unit


    val fold_ver : ('a -> vertex -> 'a) -> 'a -> t ->  'a
    val fold_edge : ('a -> edge -> 'a) -> 'a -> t ->  'a


    (*observers*)
    val size : t -> int
    val nb_edge :  t -> int

    
    val veq : vertex -> vertex -> bool
    val vcompare : vertex -> vertex ->int
    val vhash : vertex -> int
    val eeq : edge -> edge -> bool
    val ecompare : edge -> edge -> int
    val ehash : edge -> int

    (* all the folowing should be in O(1)*)
    val prede_ver : t -> vertex -> vertex list
    val succ_ver : t -> vertex -> vertex list
    val see_name : t -> vertex -> string
    val see_supply : t -> vertex -> int 
    val see_flow : t -> edge -> int
    val see_cost : t-> edge -> int
    val see_capacity : t -> edge -> capacity 

    (*for residual graphs*)
    

    type residual_edge_type  = Forward | Backward
    
    val set_residuality : t -> bool -> unit
    val is_residual : t -> bool
    val e_type : t -> edge -> residual_edge_type

            
    val add_fedge :  t ->  edge -> unit (* add forward edge to a residual graph*)
    val add_bedge : t ->  edge -> unit  (* add backward edge to a residual graph*)

    
   (* transformations*)

    val sing_source_sing_sink :  t ->  t
   
   val uncapacitated_to_capacitated :  t ->  unit
  end


module G :FLOW_GR = struct

  
    open D

    
    
    let  _  = empty 
    let _  = fill 0 0 
    type vertex = int 
    type edge = vertex *vertex
    (*1 array for the : b(i), point(i), rpoint(i),f/r-trace(i),tail,head,cost,capacity*)
    
    type vertex_attributes = {b : int array}
    type pointers = { frange : int array ; rrange : int array   ;
                      fpoint : D.t       ; rpoint : D.t        }
    type edge_attributes = { map_e_to_num : (edge,int) Hashtbl.t ; 
                        tail : D.t;
                        head : D.t;
                        cost : D.t;
                        capacity : D.t;
                        flow : D.t;
                        edge_type : D.t (*-1 for backard ; 1 for forward*)}
    
    type  t = {mutable r : bool (*residual or not*); v_a : vertex_attributes ; p : pointers ; e_a : edge_attributes  }
    type capacity = Capa of int | Infty
    type vpath = vertex list
    type epath = edge list
    
    exception Unvalid_Capacity_Operation
    exception Not_residual_graph of t
    exception Uncapacitated_arc of edge
    

    let c_op op c1 c2 = 
      match c1 with
      |Infty -> Infty
      | Capa x -> match c2 with
                  | Infty -> Infty
                  |Capa y -> Capa (op x y)
        
    let (++) = c_op (+)
    

    let ( ** ) c1 c2 = 
        let e = c1,c2 in
        if e  = (Capa 0, Infty) || e = (Infty, Capa 0 )then raise Unvalid_Capacity_Operation 
        else  c_op ( * ) c1 c2
    
    let (>>) c1 c2 = match c1 with  
        | Infty -> ( match c2 with 
                    | Infty -> false
                    | Capa _ -> true)
        | Capa x -> ( match c2 with
                     | Infty -> false
                     | Capa y -> x>y )
  
    let (--) c1 c2 = 
      if c1 = Infty && c2 = Infty  || c2 >> c1 then raise Unvalid_Capacity_Operation 
      else c_op (-) c1 c2
                   
    let get_id (g:t) (e:edge) =   Hashtbl.find g.e_a.map_e_to_num e
    
    
    
    let create n : t =
      let b = Array.make n 0 in 
      let fpoint = D.create n in
      let rpoint = D.create n in
      let frange = Array.make n (-1)  in
      let rrange = Array.make n (-1) in
      let map = Hashtbl.create n  in
      let tail = D.create n in 
      let head = D.create n in 
      let capacity = D.create n in 
      let cost = D.create n in
      let flow = D.create n in 
      let edge_type = D.create 0 in 
    
      {r = false ;v_a ={ b} ;p = {fpoint ;rpoint ;frange ;rrange }; 
            e_a = {map_e_to_num = map ; tail; head; capacity ; cost ; flow ;edge_type } }
      
    let empty = create 0
    let upper_capacity = 999
    

    let n_init g (i,j)= 
    add g.e_a.tail i ;
      add g.e_a.head j ; 
      add g.e_a.cost 0 ; 
      add g.e_a.capacity (-1);
      add g.e_a.flow 0;
      add g.p.rpoint (-1);
      add g.p.fpoint (-1)
    
    let f_init g e = 
      n_init g e;
      add g.e_a.edge_type 1
    let b_init g e = 
      n_init g e ; 
      add g.e_a.edge_type (-1)
    let general_add_edge g init_function (i,j) = 
      let m = D.size g.e_a.tail in
     
      init_function g (i,j);

      let finit_neigh i = 
        g.p.frange.(i) <- m;
        set  g.p.fpoint m  (-1)
      in 
      let rinit_neigh i = 
        g.p.rrange.(i) <- m;
        set g.p.rpoint m  (-1)
      in
      let fupdate_neigh i = 
        let start = g.p.frange.(i) in
        let next = see g.p.fpoint start in
        set g.p.fpoint start m;
        set  g.p.fpoint m next
      in
      let rupdate_neigh j = 
        let start = g.p.rrange.(j) in
        let next = see g.p.rpoint start in
        set g.p.rpoint start m;
        set  g.p.rpoint m next
      in
    
      if g.p.frange.(i) = -1 then
        finit_neigh i 
      else 
        (fupdate_neigh i);
      if  g.p.rrange.(j) = -1 then
        rinit_neigh j
      else 
        rupdate_neigh j ;
          
      Hashtbl.add g.e_a.map_e_to_num (i,j) m

    let add_fedge g e = general_add_edge g f_init  e
    let add_bedge g e = general_add_edge g b_init e 

    let add_edge  g e  =  general_add_edge g n_init e
  
    
    (*modification des pointeurs*)
        
    let set_flow g e x = 
      let id  = get_id g e in
      set g.e_a.flow id x
    
    let set_cost g e x = 
      let id  = get_id g e in
      set g.e_a.cost id x
    let set_capacity g e c = 
      let x = match c with  
        |Infty -> -1 
        |Capa x -> x
      in 
      let id  = get_id g e in
      set g.e_a.capacity id x
    
    let set_supply g v x =   g.v_a.b.(v)<- x
    
    let size g  =   Array.length g.v_a.b
    let nb_edge g = D.size g.e_a.cost 
    
    let veq = (=)
    let vcompare = Stdlib.compare
    let vhash = Hashtbl.hash
    let eeq = (=)
    let ecompare = Stdlib.compare
    let ehash = Hashtbl.hash
    
    
    (* *)
    
    let iter_ver f g = 
      let n =  size g in 
      for i = 0  to n-1 do 
        f i
      done
    let iter_edg f g  = 
      let m  = D.size  g.e_a.tail in
      for i = 0  to m-1 do
        let e  = see g.e_a.tail i , see g.e_a.head i in 
        f e
      done
    
    let fold iterator f x0 g = 
      let pre  = ref x0 in 
      let aux x = 
        let temp  =  f !pre x in 
        pre := temp 
      in 
      iterator aux g;
      !pre

    let  fold_ver f x0 g = fold iter_ver f x0 g
      
    let fold_edge f x0 g  = fold iter_edg f x0 g


    let neighbor_ver  range point  what_to_take  v = 
      let ls  = ref []  in 
      let start =  range.(v) in 
      let  k = ref start in
      while !k <> -1 do 
        let v = see what_to_take !k in 
        ls := v::!ls;
        
        k:= see point !k
      done;
      !ls
    
    let succ_ver  g (v:vertex) :vertex list = 
      neighbor_ver g.p.frange g.p.fpoint   g.e_a.head v
    let prede_ver  g (v:vertex):vertex list = 
      neighbor_ver g.p.rrange g.p.rpoint  g.e_a.tail v
    
    
    let see_name _ v = string_of_int v


  let fetch_attribute_e (g:t)  obj (e:edge) = 
      try
        let id  =  get_id g e in
        see obj id
      with
        | Not_found -> 0

    let see_supply g v  =   g.v_a.b.(v)
    let see_flow g e =
      fetch_attribute_e g g.e_a.flow e 
    let see_cost g e = fetch_attribute_e g g.e_a.cost e
    
    let see_capacity g e = 
      let x = fetch_attribute_e g g.e_a.capacity e in 
      if x>=0 then 
        Capa x 
      else
        Infty
    
    

    type residual_edge_type = Forward | Backward

    let set_residuality g x =  g.r <- x
    let is_residual g = g.r 
    let e_type  g e = if not g.r then raise (Not_residual_graph g);
      let id = get_id g e  in 
      let x  = see g.e_a.edge_type id
      in
      if x  =  -1 then Backward else Forward



  let sing_source_sing_sink g  = 
    let n   =  size g  in 
    let g' =  create (n+2) in
    let total_supply = ref  0  in 

    let unseen_v  =  Array.make n true in 
    let is_source v  = see_supply g v > 0  in 
    let is_sink v  = see_supply g v <0 in 

    let connect_to_terminal t v  = 
    (*t = 0  ->> connect to mega_source
      t = n+1 ->> connect to mega sink*)
      let b = see_supply g v  in 
      let e = if t <v+1 then (t,v+1) else (v+1,t) in 
      add_edge g' e;
      set_capacity g' e (Capa b);
      (*cost is 0 default*)
      if b > 0 then 
        total_supply :=  !total_supply+ b

    in 
    let add_source = connect_to_terminal 0  in 
    let add_sink  =  connect_to_terminal (n+1) in 
    let add_terminal v = 
      if unseen_v.(v) then 
        if is_source v then add_source v ; 
        if is_sink v  then add_sink v 
    in 


    let process_edge  (i,j) = 
      add_edge g' (i+1,j+1) ; 
      set_capacity g' (i+1,j+1) (see_capacity g (i,j));
      set_flow g' (i+1,j+1) (see_flow g (i,j));
      set_cost g' (i+1,j+1) (see_cost g (i,j));    
      
      add_terminal i ; 
      add_terminal j ;
      unseen_v.(i) <- false;
      unseen_v.(j) <- false
    in

    iter_edg process_edge g;
    set_supply g'  0 !total_supply ; 
    set_supply g' (n+1) (- !total_supply ) ;

    g'




  let uncapacitated_to_capacitated g = 
    let aux  e= 
     let c  =  match see_capacity g e with  
                  | Infty -> Capa upper_capacity
                  | x -> x 
    in set_capacity g e c
    
    in 
    iter_edg aux g 
   
  let capacitated_to_uncapacitated g  = 
    let aux e = match see_capacity g e with
      | Capa x when x = upper_capacity ->  set_capacity g e Infty
      | Capa x -> ()
      | Infty ->  raise ( Uncapacitated_arc e )
    in 
    iter_edg aux g 

 end 

module Helper = 

  struct
    open G

    
    (*capa help*)
    let min_capa c1 c2 = match c1 with
      | Infty -> c2
      | Capa x -> match c2 with 
                  | Infty -> Infty
                  | Capa y -> Capa (min x y) 

    (* path help*)
    let vpath_to_epath w  = 
      let rec aux ls accu = match ls with
        | _ :: [] | [] -> accu
        | i :: j :: tl -> aux (j :: tl) ( (i,j)::accu )
      in
      aux w []


    let  rec do_along_epath f (p:epath) = match p with
      | [] -> ()
      | hd :: tl -> f hd;
                    do_along_epath f tl

                    
    (*flow help*)
    let has_licit_flow g = 

      let mass_balance (v:G.vertex) = 
        let prede_v = prede_ver g v in
        let succ_v = succ_ver g v in 
    
      (*count in and out flow*)
        let in_flow = List.fold_left (fun c_in_flow x-> c_in_flow +  see_flow g (x,v) ) 0 prede_v in 
        let out_flow = List.fold_left (fun c_out_flow x-> c_out_flow +  see_flow g (v,x) ) 0 succ_v in
        
        out_flow - in_flow = see_supply g v
      in
    
      let nodes_balanced  = ref true  in
      iter_ver (fun x -> nodes_balanced := !nodes_balanced && mass_balance x) g ;
      
      !nodes_balanced

    let incr_fl_along_edge g delta (i,j) =  
      let new_flow = see_flow g (i,j) + delta   in 
        set_flow  g (i,j) new_flow

    (*residual graph help*)
    let to_residual_graph g =
      let n  = size g in 
      let r = create n in
      set_residuality r true;

      (*setting costs and residual capacities in a specif arc*)
      
      let make_resi_arcs (i,j) = 
         
        let x_ij = see_flow g (i,j) in        
        let c_ij = see_cost g (i,j) in   
        let u_ij = see_capacity g (i,j) in 

        let r_ij = u_ij -- Capa x_ij   in
        
        if r_ij >> Capa 0 then 
          ( add_fedge r (i,j);
            set_cost r (i,j) c_ij ;
            set_capacity r (i,j) r_ij) ;
        
        if x_ij > 0 then 
          ( add_bedge r (j,i);
            set_cost r (j,i) (- c_ij);
            set_capacity r (j,i) (Capa x_ij) );

      in
        (*iters through all edges and create the corresponding arc in the residual graph *)
      iter_edg make_resi_arcs g;
      r
    exception Uncapacitated_arc_found of edge
    exception Zero_cost_arc_found of edge
    let from_residual_graph r =  (* no uncapacitated arc*)
      
      if  not ( is_residual r ) then raise ( Not_residual_graph r ) ;  
      let n  = size r in 
      let g  =  create n in
      let seen  =  Hashtbl.create (n*n)  in 

      let capa_to_int e = function
        | Infty -> raise (Uncapacitated_arc_found e)
        | Capa x  -> x 
      in 

      (*setting cost and capacities in g graph*)
      let make_arc ((i,j):edge) = 
        if not ( Hashtbl.mem seen (i,j) ||  Hashtbl.mem seen (j,i) ) then

          let r_ij = see_capacity r (i,j)   in 
          let r_ji = see_capacity r (j,i)   in (* should be 0 default if edge doesnt exist  *)
          let rc_ij = see_cost r (i,j)      in 
          let rc_ji = see_cost r (j,i)      in 
       
         if r_ij <>Capa 0 || r_ji <> Capa 0 then
          
           ( let f_e  = match e_type r (i,j) with
                  | Forward ->  (i,j) 
                  | Backward -> (j,i)
            in 
              let b_e = match f_e with 
                      | (a,b) -> (b,a)
            in
            
            let u = r_ij ++ r_ji           in
            let x  =  capa_to_int b_e (see_capacity r b_e) in 
            let c =    (max ( abs rc_ij ) ( abs rc_ji ) ) in
      
            add_edge g f_e;
            set_capacity g f_e u;
            set_cost g f_e c ;
            set_flow g f_e x ) 
          ;
       
          Hashtbl.add seen (i,j) true ; 
          Hashtbl.add seen (j,i) true 
      in
      iter_edg make_arc r;
      g 

    let convert_flow_to_residual g r flow0=
      let make_resi_flow (i,j) = 
        let x_ij = see_flow g (i,j) in  
        let x_ij0 = flow0 (i,j) in
        
        let x_ij' = if x_ij > x_ij0 then x_ij - x_ij0 else 0 in
        let x_ji' = if x_ij > x_ij0 then 0 else x_ij0 - x_ij  in
          
        set_flow r (i,j) x_ij';
        set_flow r (j,i) x_ji';
      in

      iter_edg make_resi_flow g

    let convert_flow_from_residual g r flow0 =

      let make_norm_flow (i,j) = 
        let x_ij' = see_flow r (i,j) in
        let x_ji' =  see_flow r (j,i) in 
        let x_ij0 = flow0 (i,j) in

        let x_ij = x_ij' - x_ji' + x_ij0 in 

        set_flow g (i,j) x_ij
      in

      iter_edg make_norm_flow g

                        
(**)
  let creation_wizard () = 
    let  g = ref empty in 
    
    let () = print_string "Creation Wizard. \n
                            Enter graph size  \n" in
    let  n = read_int () in 
    g := create n ;
    
    for i = 0 to  n-1 do 
      let () = print_endline ("\n Set supply for vertex " ^ string_of_int i ^ " : ") in 
      let b = read_int () in 
      set_supply !g i b;
      print_newline () 
    done;
    let () = print_endline "\n add an edge or dismiss (e/d) \n " in
    let x  =  ref (read_line ()) in  
    
    while !x <> "d" do  
      let () = print_endline "\n Tail: " in 
      let t = read_int () in 
      let () = print_string " Head : " in 
      let h  = read_int () in 
      let () = print_string " Capa : " in 
      let cap  = read_int () in  
      let () = print_string " cost : " in 
      let cost  = read_int () in
      let () = print_string " flow : " in 
      let flo  = read_int () in
      
      let e = (t,h) in 
      add_edge !g e;
      set_capacity !g e (Capa cap) ; 
      set_cost !g e cost ; 
      set_flow !g e flo;

      let () = print_endline "\n add an edge or dismiss (e/d) \n " in
      x :=  (read_line ())
    done ; 
    
    let () = print_endline "\n Graph defined." in 
    let is_ok = has_licit_flow !g in 
    let () =  if is_ok then 
                print_endline "The flow respects the mass balance contraint"
             else
                print_endline " The flow does not respect the mass balance contraint"
    in 
    let ()  = print_endline " enter anything to terminate" in 
    let _ = read_line () in
    !g


  end



module type STD_ALG = 
  sig
    open G 
    val maximum_flow : t ->  t 
    val neg_cycle : t -> epath

  end

module Std_alg = 
  struct
    open G
    exception Uncapacitated_negcost_cycle
    exception Break
    open Stack
    let dft g f  (v0:vertex) ?goal:(goal= -1) = 
      let n =size g  in  
      let prede_ver =  Array.make n (-1) in 
      let ancestor =  ref (-1) in  
      let to_come  = Stack.create ()  in
      push to_come v0 ; 

    try
      while not (is_empty to_come ) do 
        let i = pop to_come in 
        f i ;
        prede_ver.(i) <- !ancestor; 
        ancestor := i ;
        List.iter (fun x -> push to_come x) (succ_ver g i);
        
        if i =  goal then raise Break

        done;
      
      prede_ver
    with
      | Break -> prede_ver

    let maximum_flow _ =  G.create 0
    let neg_cycle _ = []
  end



module Miscellaneous = 
  struct 
    let list_min ls =
       let rec  aux ls accu  = match ls with
        | []  -> accu
        | hd::tl ->   if hd < accu then aux tl hd
                      else aux tl accu
        in
        aux ls (List.hd ls) 

  end



module type X_graph = sig
  val input_g : G.t
end 

module Display (M:X_graph) = 
(*obj :  writing a function that generates a dot file of a g:G.t graph*)
  struct
    let g0 = M.input_g
    let get_vertex_label g (v:G.vertex) = 
      let supply = string_of_int (G.see_supply g v) in
       
       G.see_name g v ^ ": b"^supply

    let get_edge_label g (e:G.edge) = 
      let capa = match G.see_capacity g e with 
                  | Infty -> "/Infty"
                  | Capa x ->"/" ^ (string_of_int x)
      in
      string_of_int (G.see_flow g e ) ^ capa ^"~"^(string_of_int (G.see_cost g e))


    module Ver  = struct
      type t = G.vertex
    end
    module Edg = struct
      type t = G.edge
      let src ((i,_):G.edge) = i 
      let dst ((_,j):G.edge) = j  
    end

    module X = struct
      type t = G.t
      module V = Ver
      module E = Edg
      let iter_vertex = G.iter_ver 
      let iter_edges_e = G.iter_edg

      let graph_attributes _ = []
      let default_vertex_attributes _ = []
      let vertex_name  = G.see_name g0
      let vertex_attributes v = [ `Label (get_vertex_label g0 v)] 
      let get_subgraph _ = None 
      let default_edge_attributes _ = []
      let edge_attributes (i,j) = [`Label (get_edge_label g0 (i,j) ); (* `Penwidth (float_of_int (G.see_flow g0 (i,j))) *)]

    end
    
    module Print = Graph.Graphviz.Dot (X)

    let generate () = 
      let _  = 
        let file = open_out_bin "flow_graph.dot" in
          Print.output_graph file g0 in
      ()

  end 

module Main = 
  struct
    open G
    open Helper
    open Std_alg
    exception Infeasible_flow
    let cycle_canceling g0 =
      
      let g = sing_source_sing_sink g0 in
      let g' = maximum_flow g in
      
      if g' = empty then
        raise Infeasible_flow
      else
        let r  = ref (to_residual_graph g') in 
        let c = ref (neg_cycle r) in 
        while !c <> [] do
          let w = vpath_to_epath (!c) in 
          let rcapacities_w = List.map ( see_capacity !r ) w in 
          let min_w = List.fold_left min_capa Infty rcapacities_w in 
          (*/!\ erreur ptet*)
          (*since we assumed that G contains no uncapa neg cost cycle *)
          let delta =   match min_w with
                          | Infty -> raise Uncapacitated_negcost_cycle
                          | Capa x -> x 
          in
          do_along_epath (incr_fl_along_edge g' delta) w;
          r := to_residual_graph g';
          c := neg_cycle r

        done ;
        g'
  

  end



let test ()  = 
  let g = G.create 4  in 

  G.add_edge g (0,2); 
  G.add_edge g (0,1);
  G.add_edge g (1,2); 
  G.add_edge g (2,3);
  G.add_edge g (1,3);
  
  G.set_flow g (0,2) 3;
  G.set_flow g (0,1) 2;
  G.set_flow g (1,2) 2;
  G.set_flow g (2,3) 5;
  G.set_flow g (1,3) 0;

  G.set_capacity g (0,2) (Capa 4);
  G.set_capacity g (0,1) (Capa 2);
  G.set_capacity g (1,2) (Capa 3);
  G.set_capacity g (2,3) (Capa 5);
  G.set_capacity g (1,3) (Capa 1);

  G.set_cost g (0,2) 3;
  G.set_cost g (0,1) 2;
  G.set_cost g (1,2) 2;
  G.set_cost g (2,3) 5;
  G.set_cost g (1,3) 0;

  G.set_supply g 1 9;
  
  let _ = read_int () in 
  
  g

let g =test () 
 let g'= G.sing_source_sing_sink g 

module  X :X_graph  = struct let input_g = g' end
module Dis  =  Display (X) 

let () = Dis.generate () 