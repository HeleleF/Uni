package de.htw_berlin.ai_bachelor.kbe.tree;

/**
 * summiert alle Werte aller Knoten einer Baumstruktur, 
 * indem rekursiv aufgerufen wird
 * 
 * d) nur mit Integer-Trees benutzbar
 */
public class TreeSum {
	
	public static Integer sum(Tree<Integer> t) { // d) vllt. auch "Tree<? extends Integer>" ???
		
		int result = 0;
		
		if (t != null) {
			result = t.getValue() + sum(t.getLeft()) + sum(t.getRight());
		}
		return result;
	}
}
