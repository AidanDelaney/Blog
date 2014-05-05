# Home Office Server Configuration #

I use my home office server mainly for backup, personal email and hosting my blog.  The backup functionality is of most importance to me and comes in two flavours:

  * instant backup of high-importance documents using [Sparkleshare](http://www.sparkleshare.org), and
  * weekly backup of everything using [déja-dup](https://launchpad.net/deja-dup).

As such, the only services I need running on the server are

  * a http server,
  * a mail server, and
  * an ssh server.

Furthermore, I've now automated the provisioning of these.  That is, assuming a base OS and an ssh server are functioning.

I've also had to move the server from my back room to a more visible part of the house, due to some dodgy wiring.  This meant decomissioning my old, faithful, passively cooled Celeron machine in favour of a RaspberryPi.  The Celeron box, though passively cooled, needed a fan and a decent sized case.  The RaspberryPi simply needs a USB attached storage drive (I'd love to see SATA on board the RPi).

Most of the RPi config is delivered via Puppet.  I still don't know of a good puppet MTA module.  I'd prefer exim, but if there was a module that _just worked_, I don't care what MTA it might use underneath.