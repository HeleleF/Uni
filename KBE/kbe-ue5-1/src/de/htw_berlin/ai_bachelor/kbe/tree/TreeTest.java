package de.htw_berlin.ai_bachelor.kbe.tree;

import java.util.ArrayList;
import java.util.Collection;

public class TreeTest {
		
	public static void main(String... a) {
		Tree<Integer> testbaum = new Tree<>(1,new Tree<>(23, new Tree<>(65), null),new Tree<>(42, null, null));
		
		Tree<Integer> t1 = new Tree<>(14,null,null);
		Tree<Integer> t2 = new Tree<>(15,null,null);
		Tree<Integer> t3 = new Tree<>(7,t1,t2);
		Tree<Integer> t4 = new Tree<>(12,null,null);
		Tree<Integer> t5 = new Tree<>(13,null,null);
		Tree<Integer> t6 = new Tree<>(6,t4,t5);
		Tree<Integer> t7 = new Tree<>(10,null,null);
		Tree<Integer> t8 = new Tree<>(11,null,null);
		Tree<Integer> t9 = new Tree<>(5,t7,t8);
		Tree<Integer> t10 = new Tree<>(8,null,null);
		Tree<Integer> t11 = new Tree<>(9,null,null);
		Tree<Integer> t12 = new Tree<>(4,t10,t11);
		Tree<Integer> t13 = new Tree<>(3,t6,t3);
		Tree<Integer> t14 = new Tree<>(2,t12,t9);
		Tree<Integer> root = new Tree<>(1,t14,t13);

		
	
		System.out.println(export(testbaum));
		System.out.println(export(root));
		System.out.println();
		
		// d)
		System.out.println("Summe Baum: " + TreeSum.sum(testbaum));
		
		// g)
		ComparePredicate<Integer> comptest = new ComparePredicate<>(20, 50);
		ComparePredicate<Integer> comptest2 = new ComparePredicate<>(2, 9);
		
		System.out.println("Filter...");
		System.out.println(filterElements(comptest, testbaum));
		
		System.out.println("Filter...");
		System.out.println(filterElements(comptest2, root));
		
		
	}
	
	// c) (([Blatt11] Knoten1 [Blatt12]) Root ([Blatt21] Knoten2 [Blatt22])) entspricht:
	
	
	//		   		  Root
	//				   /\
	//				  /  \
	//		   		 /    \
	//		  		/      \
	//		 Knoten1        Knoten2
	//     	/\                    /\                 
	//     /  \                  /  \               
	//    /    \                /    \                       
	//Blatt11   Blatt12  Blatt21  Blatt22
	
	
	public static String export(Tree<?> t) {

		if (t != null) {

			if (t.getLeft() == null && t.getRight() == null) {
				return "[" + t.getValue() + "]";
			} else {
				return "(" + export(t.getLeft()) + " " + t.getValue() + " " + export(t.getRight()) + ")";
			}
		}
		return "";
	}
	
	// g) nur V's, die Comparable<V> implementieren
	public static <V extends Comparable<V>> Collection<V> filterElements(ComparePredicate<V> comp, Tree<V> test2) {

		// speichert alle Rueckgabewerte in ArrayListe vom Typ V
		ArrayList<V> alle = new ArrayList<>();

		if (test2 != null) {
			if (comp.isOk(test2)) {
				alle.add(test2.getValue());
			}

			if (test2.getLeft() != null) {
				alle.addAll(filterElements(comp, test2.getLeft()));

			}

			if (test2.getRight() != null) {
				alle.addAll(filterElements(comp, test2.getRight()));
			}
		}

		return alle;

	}
}
