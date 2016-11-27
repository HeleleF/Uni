package de.htw_berlin.ai_bachelor.kbe;

import java.io.Serializable;
import java.util.Arrays;

@CreationInfo(description="Diese Klasse ist sinnlos", author="wie heiﬂt der sani", tags={"Annos","Test"}, baseclass = true, interfaces=true)
public class MyAnnotationTester extends MyTester implements Serializable, Comparable<Object> { //siehe c), siehe f)

	private static final long serialVersionUID = 1L;

	public static void main(String[] args) { //siehe d)
		
		CreationInfo creationInfo = MyAnnotationTester.class.getAnnotation(CreationInfo.class);

		if (creationInfo != null) 
			printCreationInfo(creationInfo);
		else
			System.out.println("Keine " + CreationInfo.class.getSimpleName() + "-Annotation vorhanden.");
	}

	private static void printCreationInfo(CreationInfo creationInfo) {
		
		System.out.println("author: " + creationInfo.author());
		System.out.println("description: " + creationInfo.description());
		System.out.println("tags: " + Arrays.toString(creationInfo.tags()));
		
		if (creationInfo.baseclass()) { //siehe g)
			System.out.println("Superklasse: " + MyAnnotationTester.class.getSuperclass().getName());
		}
		if (creationInfo.interfaces()) { //siehe g)
			System.out.println("Interfaces: " + Arrays.toString(MyAnnotationTester.class.getInterfaces()));
		}
	
	}

	public int compareTo(Object o) { 
		// TODO Auto-generated method stub
		return 0;
	}
}
