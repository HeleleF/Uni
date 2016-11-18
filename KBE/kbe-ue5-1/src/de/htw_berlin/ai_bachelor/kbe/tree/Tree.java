package de.htw_berlin.ai_bachelor.kbe.tree;

/**
 * Baumstruktur
 * jeder Knoten hat einen Wert, einen linken Teilbaum
 * und einen rechten Teilbaum
 * 
 * c)
 */
public class Tree<T> {
	
	private final T value;
	private final Tree<T> left;
	private final Tree<T> right;

	public Tree(T v) {
		this(v, null ,null);
	}
	
	public Tree(T v, Tree<T> l, Tree<T> r) {
		this.value = v;
		this.left = l;
		this.right = r;
	}

	public T getValue() {
		return this.value;
	}

	public Tree<T> getLeft() {
		return this.left;
	}

	public Tree<T> getRight() {
		return this.right;
	}
}
