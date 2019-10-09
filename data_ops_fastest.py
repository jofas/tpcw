import sys
import re
import os
import sqlite3
import numpy as np

from statistics import median, mean

MATCH = re.compile(r"(^Threads: (?P<threads>[0-9]+)|^.*(?P<loop>[12]) = +(?P<time>[0-9]+.[0-9]+))")

def parse():

    try:
        os.remove("data/clean_fastest.db")
    except:
        pass

    con = sqlite3.connect("data/clean_fastest.db")

    con.execute("""
        CREATE TABLE data (
            threads TEXT,
            loop    INT,
            time    REAL
        );
    """)

    data = []
    cur_threads = None

    with open("data/raw_fastest.txt", "r") as f:
        for line in f:
            s = re.match(MATCH, line)

            if s != None:
                if s.group('threads') != None:
                    cur_threads = s.group('threads')
                else:
                    loop = int(s.group("loop"))
                    time = float(s.group("time"))
                    data.append([cur_threads, loop, time])

    for row in data:
        con.execute("""
            INSERT INTO data (threads, loop, time)
                VALUES ("{}", {}, {});
        """.format(*row))

    con.commit()
    con.close()

def extract_mean_and_median():
    con = sqlite3.connect("data/clean_fastest.db")

    cur = con.execute("""
        SELECT DISTINCT threads FROM data;
    """)

    Xl1, Xl2, threads = [], [], []

    for row in cur:
        thread = row[0]

        l1 = [x[0] for x in con.execute("""
            SELECT time FROM data WHERE threads="{}"
                AND loop=1;
        """.format(thread))]

        l2 = [x[0] for x in con.execute("""
            SELECT time FROM data WHERE threads="{}"
                AND loop=2;
        """.format(thread))]

        Xl1.append([mean(l1), median(l1)])
        Xl2.append([mean(l2), median(l2)])
        threads.append(thread)

    con.close()
    return np.array(Xl1), np.array(Xl2), np.array(threads)


def meta():
    Xl1, Xl2, threads = extract_mean_and_median()

    print(threads)

    print("Loop 1")
    print(Xl1)

    print("Loop 2")
    print(Xl2)


def export_tables():
    Xl1, Xl2, threads = extract_mean_and_median()

    header_row = "\#threads" + "".join(["&{} ".format(t) for t in threads] + ["\\\\"])

    for i, X in enumerate([Xl1, Xl2]):
        loop = i + 1

        mean_row = "mean " + "".join(["&{:.2f} ".format(v) for v in X[:,0]] + ["\\\\"])
        median_row = "median " +  "".join(["&{:.2f} ".format(v) for v in X[:,1]] + ["\\\\"])

        with open("doc/fastest_meta_loop%s.tex" % loop, "w") as f:
            f.write("""
                \\begin{tabu}{l|XXXXXXX}
                %s
                \\hline
                %s
                %s
                \\end{tabu}
            """ % (header_row, mean_row, median_row))

if __name__ == "__main__":
    if sys.argv[1] == "parse":
        parse()
    elif sys.argv[1] == "meta":
        meta()
    elif sys.argv[1] == "export":
        if sys.argv[2] == "tables":
            export_tables()
