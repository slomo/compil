Probleme und Lösungen
=====================

Typen ermitteln
----------------

Das große Problem zur Zeit betrifft Situationen in denen das typing nicht
zuverlässig funktioniert. Ein Beispiel für eine solche ist die Verwendung des
fixpunkt operators:

::

    ((fix
        (lambda (f x)
            (cond (= x 0)
                1
                (f (- x 1))
            )
        )
    ) 3)

In diesem Fall kann mein jetztiger Algorithmus den Typ von x ermitteln (zum
Beispiel aus dem Vergleich mir null, schlußfolgern das es wohl eine Zahl ist).
Was er nicht leisten kann den Typen von f zu ermitteln. Obwohl sowohl der
Returntyp (scheint eine valide Ersatz für 1 zu sein) und die Parameter (
defintiv int erraten werden könnten).

Mein jetziger Algorithmus schließt zwar von einem cond Zweig auf den anderen,
aber nicht von den Parametern auf die Funktion, so das er zumindest letzteres
nicht weiß.

Als Lösung würde ich eine Modifikation des Algorithmus vorschlagen. Der
Algorithmus sollte im ersten Schritt alle Abnhängigkeiten der Typen in einem
Graph sammeln, und zwar sowohl in Funtkion zu Paramter als auch in Paramter zu
Funktionsrichtung. Und dann auf diesem Graphen von den Literalen ausgehen die
Typen analysieren. Damit müssten durch Schließen aus beiden Richtungen
konfliktefreie Typen entstehen, so lange eine Typsierung überhaupt möglich ist.

Es gibt auch einfachere Modifikationen des Algorithmus, aber auch solche
benötigen mehr Zeit, als ich noch für das Projekt habe zur Umsetzung.

LLVM-Generierung
----------------

Ein zweites Problem ist die Generierung des LLVM-Code. Sie funktioniert zwar
soweit einwandfrei, aber es sind 300 Zeilen sehr unübersichtlicher Code. Ich
hab zweimal versucht das in eine Libary auzulagern, aber keine geeignete
Abstraktionsschicht gefunden auf die Schnelle. Mit etwas überlegung ließe sich
sicher eine API finden, die den vorhanden Code deutlich verschlanken würde.

