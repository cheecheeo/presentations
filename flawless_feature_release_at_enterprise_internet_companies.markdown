% Flawless feature release at enterprise internet companies
% John Chee
% Software Engineer in San Francisco, CA | @chee1bot on Twitter

What's the problem?
-

- We have a change (new feature or bugfix)

. . .

- We want every customer to have that change

<center>![Happy User](./happy_user.jpeg)</center>

The easy solution
-

- Once upon a time we could do something simple
    - Our application ran on a single computer
        - Change a file
        - Restart the program
        - Worst case: restart the whole computer

. . .

- Downtime was accepted

<center>![config.ini](./config_ini.jpg)</center>

It's not that easy anymore
-

- Consumer Internet companies measure downtime in dollars lost

. . .

- Customers will flee
    - From the sites that are down
    - To their competitors that are still online

Many applications are BIG now
-

- Enterprise applications run on many machines (> 1 and < ???)

<center>![Datacenter](./datacenter.png)</center>

The refined problem
-

- Deliver a feature to hundreds of millions of users

. . .

- Minimize downtime
- The application runs on 1000s of machines
- Enabling or disabling of the feature should happen instantly

Enterprise Architecture
-

- Getting many computers to agree requires a well architected system

. . .

- A problem that has been studied for a while
    - Lamport, L.; Shostak, R.; Pease, M. (1982). "The Byzantine Generals Problem"

. . .

- Software engineers want an "engineer-friendly" version
    - Google has "Chubby"
        - Google Research Publication: Chubby Distributed Lock Service. research.google.com
    - Apache has "Zookeeper"
        - [zookeeper.apache.org](http://zookeeper.apache.org)

. . .

- Failures happen in practice!

<center>![Mess of cables](./cablemess-2.jpg)</center>

Operational Execution Excellence
-

- Ideal customer impact

. . .

- The customer should only notice exactly what we want

. . .

- Ideally:
    - No additional loading time
    - No requirement to logout and login

. . .

- At first we only want a fraction of users to experience the change
    - Over time we want more and more users to experience the change
    - If there's a problem we need to instantly disable the change

Questions?
-

