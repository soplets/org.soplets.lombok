org.soplets.lombok
==================

SOPLETS is a Java coding pattern which allows you organizing the logics contained in your code in a consistent, systematic way by just using annotations and enumerations.

This is the library to support the  Java SOPLETS coding pattern.
Find out more at http://www.soplets.org/

Build the jar with 'gradle jar'
lombok_soplets requires lombok (http://projectlombok.org/)
It has been successfully tested with lombok-0.11.6.

To develop using eclipse modify the file eclipse.ini inside your eclipse home folder.
At the end of the file add the following lines with the correct path to the needed jars:

-javaagent:/<PATH-TO-JAR>/lombok-0.11.6.jar
-Xbootclasspath/a:/<PATH-TO-JAR>/lombok-0.11.6.jar
-Xbootclasspath/a:/<PATH-TO-JAR>/lombok_soplet.jar
