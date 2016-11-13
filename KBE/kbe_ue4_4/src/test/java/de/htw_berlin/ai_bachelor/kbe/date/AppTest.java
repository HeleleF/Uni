package de.htw_berlin.ai_bachelor.kbe.date;

import static org.junit.Assert.*;
import org.junit.Test;

/**
 * Unit test for simple App.
 */
public class AppTest {
	private Datum testdatum = new Datum(18,11,2001);
	private DatumFormatierer testformatierer;

    @Test
    public void testFormatiererISO() {
    	testformatierer = new DatumFormatiererISO();
        assertEquals("2001-11-18", testformatierer.format(testdatum));
    }
    
    @Test
    public void testFormatiererCH() {
    	testformatierer = new DatumFormatiererCH();
        assertEquals("18.11.2001", testformatierer.format(testdatum));
    }
    
    @Test
    public void testFormatiererUS() {
    	testformatierer = new DatumFormatiererUS();
        assertEquals("11/18/2001", testformatierer.format(testdatum));
    }
    
}
