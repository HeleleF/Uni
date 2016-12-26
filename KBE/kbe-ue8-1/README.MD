#  Vorarbeit damit es nicht rumbuggt

- altes 3-1 Projekt importieren
- per Rechtsklick Refactor Rename in kbe-ue8-1 umbenennen
- in pom.xml alle "kbe-ue3-1" in "kbe-ue8-1" umbennen
- das in pom.xml einfügen:
```
  <build>
    <finalName>kbe-ue8-1</finalName>
    		<plugins>
			<plugin>
				<groupId>org.apache.maven.plugins</groupId>
				<artifactId>maven-compiler-plugin</artifactId>
				<version>2.3.1</version>
				<configuration>
					<source>1.8</source>
					<target>1.8</target>
				</configuration>
			</plugin>
		</plugins>
  </build>
```
- dann Rechtsklick auf Projekt->Maven->Update Projekt  


- bei Fehlern: Rechtsklick auf Projekt -> Properties->Project Facets und dann Dynamic WebModule auf 3.0 ändert, applyen; Java auf 1.8 ändern, applyen, Java Server faces ventl auch noch; dann nochmal Rechtsklick auf Projekt->Maven->Update Projekt

- immer wieder maven update und refresh
- dann maven clean und maven install  


- falls ein Maven Install failed, wegen Java jdk jre Gedöns:
- Rechtsklick auf das Projekt->Properties->Java Build Path ->JRE anklicken->Edit->Alternate JRE->Apply  


- bei TomcatServer Fehlern: -> In Servers in die server.xml-> doppelte context tags löschen  


- folgendes aus der faces-config.xml löschen (siehe Aufgabenstellung):
```
<application>
	<message-bundle>vvskMessages_de</message-bundle>
</application>
```

# Aufgabenblatt 8
# a)

- editToDoList.xhtml anpassen: man soll es ändern können, also InputText

- in ToDo kommt als Feld dueDate hinzu mit getter und setter; wird initialisiert mit heute + 1
- required True weil man muss ja etwas eingeben
- Pattern so wie er es wollte: t.m.jjjj

```
<h:column>
	<f:facet name="header">
		<h:outputText value="TERMIN" />
	</f:facet>
	<h:inputText id="dueDate" value="#{todo.dueDate}" required="true">
		<f:convertDateTime type="date" pattern="d.M.yyyy" />  
	</h:inputText>
</h:column>
```

- siehe https://www.mkyong.com/jsf2/jsf-2-convertdatetime-example/


# b)


- folgendes in die web.xml packen (war schon da, aber auskommentiert)
- zeigt messages auf der Seite an, auch wenn keine h:message tags da sind
- https://community.oracle.com/blogs/driscoll/2009/09/28/jsf-20-reminder-project-stage

```
<context-param>
	<param-name>javax.faces.PROJECT_STAGE</param-name>
	<param-value>Development</param-value>
</context-param>
```

- ansonsten werden auf der Konsole Warnungen angezeigt:
- er sagt Messages wurden generiert, aber nicht angezeigt, was offensichtlich blöd ist

# c)

- das kommt jetzt dazu in die editToDoList.xhtml
- extra eine neue Spalte dafür machen nach Aufgabenstellung; schön in Rot

```
<h:column>
	<h:message for="dueDate" style="color:red"/>
</h:column>
```

# d)

- Deutsch als Standardsprache einstellen:

- in die faces-config.xml einfügen:

```
<application>
	<locale-config>
		<default-locale>de_DE</default-locale>
	</locale-config>
</application>
```

- Internationaliserung: Syntax ist messages_<locale>.properties

- Datei messages_de_DE.properties anlegen in Ordner resources/de/htw_berlin/ai_bachelor/kbe/checklist8

- Die Fehlermeldungen um die es geht, sind:

```
javax.faces.converter.DateTimeConverter.DATE
javax.faces.converter.DateTimeConverter.DATE_detail
javax.faces.component.UIInput.REQUIRED
```

- Was sie sonst noch tun müssen:
- damit es klappt, muss noch folgendes in die faces-config.xml

```
<application>
	<message-bundle>
		de.htw_berlin.ai_bachelor.kbe.checklist8.messages
	</message-bundle>
</application>
```

- praktisch damit er weiß, das message existiert und wo

- siehe http://stackoverflow.com/questions/2668161/internationalization-in-jsf-when-to-use-message-bundle-and-resource-bundle

# e)

- in editToDoList.xhtml einfügen:

- Funktionalität kommt zustande durch immediate=true
- der Navigationstring sorgt dafür, das wir direkt zu Render Respone gehen
- Validierung tritt also gar nicht erst auf, Das Model wird nicht geupdatet
- die Daten werden einfach weggemüllt, damit also perfekt für nen Cancel Button
- siehe https://myfaces.apache.org/wiki/core/user-guide/jsf-and-myfaces-core-concepts/how-the-immediate-attribute-works.html

```
<h:commandButton value="Abbrechen" action="#{toDoListMB.cancel}" immediate="true"/>
```

- damit der Navigationstring funktioniert, haben wir in faces-config.xml
einen navigation-case angelegt mit dem ziel editToDoList

# f)

- folgendes in Todo.java:
```
import javax.validation.constraints.Future;
```

- damit haben wir Zugriff auf die Annotation @Future, die wir zum dueDate packen

- siehe http://docs.oracle.com/javaee/6/tutorial/doc/gircz.html

- folgendes muss noch in die pom.xml als Abhängigkeit:

```
<!-- Required for bean validation -->
<dependency>
	<groupId>javax.validation</groupId>
	<artifactId>validation-api</artifactId>
	<version>1.1.0.Final</version>
</dependency>

<!-- Reference implementation of bean validation -->
<dependency>
	<groupId>org.hibernate</groupId>
	<artifactId>hibernate-validator</artifactId>
	<version>5.2.2.Final</version>
</dependency>
```

# g)

- Syntax ist ValidationMessages_<locale>.properties

- Datei ValidationMessages_de_DE.properties anlegen im resources Ordner 

- siehe http://download.oracle.com/otndocs/jcp/bean_validation-1.0-edr-oth-JSpec/
und http://stackoverflow.com/questions/10915770/localization-with-bean-validation-in-jsf

- Die Fehlermeldung um die es geht, ist:
```
javax.validation.constraints.Future.message
```

# h)

- eine Javaklasse Interval.java erstellen (also praktisch wie Todo.java)
- Felder min und max als Int, mit getter und setter, und Konstruktor

```
import javax.validation.constraints.Min;  
```  


- gibt uns die Annotation @Min
- Max darf ja nicht kleiner als 1 sein, weil Min ja immer 1 sein muss  


- dann in Todo.java ein neues Feld prioritaet als Int
- wird mit 1 initialisiert nach Aufgabenstellung
-getter und setter für prioritaet  


- dann eine MangedBean dafür erstellen IntervalMB.java (praktisch die TodoListMB.java)
- für alle Sessions -> ApplicationScoped also  


- dann ein Facelet editInterval.xhtml anlegen  


- braucht ein form-Tag mit einem InputText Feld, wo man max eingeben kann
- ich hab noch ein Input Feld gemacht für min, was aber disabled=true ist
- zukunftssicher etc., falls man das mal ändern möchte, geht das easy
- aber die Aufgabenstellung ist trotzdem erfüllt  


- dann noch 2 CommandButtons für Speichern und Abbrechen, die beide zu editToDoList.xhtml gehen
- dabei ist der Abbrechen Button wieder immediate=true, wie oben erläutert  


- für die Validierung von max:
- 1. Datei messages_de_DE.properties erweitern:
- Die Fehlermeldungen um die es geht, sind:

```
javax.faces.converter.IntegerConverter.INTEGER
javax.faces.converter.IntegerConverter.INTEGER_detail  
```  


- weil jsf automatisch erkennt, das der eingebene Wert ein Integer sein muss wegen der ManagedBean,
- wird, falls man z.b. Buchstaben eingibt, der IntegerConverter benutzt  


- dessen Standardfehlermeldung wird hier überschrieben
- 2. Datei ValidationMessages_de_DE.properties erweitern:
- Die Fehlermeldungen um die es geht, sind:
```
javax.validation.constraints.Min.message
javax.validation.constraints.NotNull.message
```  


- dazu muss das Feld max in Interval.java noch mit @NotNull annotiert werden
- es wird also auf Not Null und auf Mindestens 1 geprüft
- die Standardfehlermeldungen defür werden hier überschrieben  


- getMin() hat keine Annotationen, weil das Input Feld ist ja disabled 

# i)

- folgendes in die editToDoList.xhtml einfügen:

- jede prio kann editiert werden, also InputText

```
<h:column>
	<f:facet name="header">
		<h:outputText value="PRIORITAET" />
	</f:facet>
	<h:inputText id="prioritaet" value="#{todo.prioritaet}" required="true">
	</h:inputText>
</h:column>
			 
<h:column>
	<h:message for="prioritaet" style="color:red"/>
</h:column>
```

# j)

- folgendes zwischen den <h:inputText> Tags einfügen:

```
<f:validateLongRange minimum="1" maximum="#{intervalMB.max}" />
```

# k)

- die Zeile 

```
<f:validateLongRange minimum="1" maximum="#{intervalMB.max}" />
``` 

wieder wegmachen

- siehe http://www.jdev.it/custom-jsr-303-bean-validations-jsr-310-new-datetime-api/

- wir brauchen eine Annotation @MyInterval.java
- nur auf Felder anwendbar -> @Target(ElementType.FIELD)

- sieht dann so aus:
```
@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.FIELD)
@Constraint(validatedBy = MyIntervalConstraint.class)
public @interface MyInterval {

	String message() default "{de.htw_berlin.ai_bachelor.kbe.checklist8.validierung.MyInterval}";
	
	Class<?>[] groups() default {};
	Class<? extends Payload>[] payload() default {};
	
	String interval();
}
```
- die ersten 3 Parameter müssen da sein, sonst gibt es Exceptions  


- message dient dazu, die Fehlermeldung zu customizen (sieh Aufgabenstellung)
- l18n war sogar gar nicht gefragt, wurde aber trotzdem gemacht (Aufgabe ist ja erfüllt)
- dazu folgendes in die ValidationMessages_de_DE.properties einfügen:  
```
de.htw_berlin.ai_bachelor.kbe.checklist8.validierung.MyInterval=Liegt nicht im definierten Intervall.    
```  


- interval Parameter ist der EL Ausdruck (@MyInterval(interval = "intervalMB.interval") in Todo.java)
- der wird vom ELResolver ausgewertet  


- die Constraint Annotation verweist auf die eigentliche Validierung:
- wir brauchen dazu einen ConstraintValidator -> MyIntervalConstraint.java  


- der overridet die Methoden isValid und initialize
- isValid prüft ob Prio >= Min und Prio <= Max, nach Aufgabenstellung
aus MyIntervalConstraint.java: "prio >= interval.getMin() && prio <= interval.getMax();"  


- man hätte anstelle von getMin auch direkt 1 nehmen können, aber wir wollten das ja flexibel machen
- wie oben schon gesagt, zukunftssicher, falls mans mal ändern will  


- initialize versucht ein Intervalobjekt zu erzeugen und greigt dazu auf die Interval Manged Bean zu mithilfe von einem ELResolver (siehe Tipp 1)  


- damit das geht, brauchen wir die java ee7 api (sieh Tipp 2)
- also folgendes in die pom.xml eintragen als Abhängigkeit:
```
<!-- die neue Javax Api fuer ELresolver letzte Aufgabe -->
<dependency>
	<groupId>javax</groupId>
	<artifactId>javaee-api</artifactId>
	<version>7.0</version>
</dependency>  
```  


- dann können wir getValue() das Objekt ranholen 
- siehe http://docs.oracle.com/javaee/7/api/javax/el/ELResolver.html

- am Anfang ist obj null, weil wir die TopLevel variable wollen
- nach getValue(Context, null, IntervalMB) ist obj also jetzt die Interval Managed Bean

- jetzt kommt getValue(Context, IntervalMB, interval), obj wird zu einem Intervalobjekt

- mit instanceof noch prüfen ob es passt, dann Objekt in Interval casten
