package de.htw_berlin.ai_bachelor.kbe.tree;

// f) ueberprueft, ob der Wert eines Baumknotens zwischen einer Min und Max Grenze liegt
// da Interface Comparable implementiert wird, ware compareTo() sinnvoll
public class ComparePredicate<V extends Comparable<V>> implements TreePredicate<V> {
	
	private V minEle;
	private V maxEle;
	
	public ComparePredicate(V minEle, V maxEle) {
		this.minEle = minEle;
		this.maxEle = maxEle;
	}
	
	@Override
	public boolean isOk(Tree<V> knoten) {
		
		V wert = knoten.getValue();
		
		boolean check = minEle.compareTo(wert) < 0 && maxEle.compareTo(wert) > 0; 
				
		return check;
	}
}
