MTB v2 communication library
============================

---

**This library is deprecated as MTB v2 was replaced with MTB v4.**

---

MTB library is used to set and get state of [MTB v2 modules](https://mtb.kmz-brno.cz/cz/v2/index)
on model railroad via
[simple `dll` interface](https://github.com/kmzbrnoI/mtb-lib/wiki/api-specs).
It supports MTB-UNI and MTB-TTL modules with firmware 4.1.

This library is used as part of the [RCS project](https://hjop.kmz-brno.cz/rcs).

## Installation

Simply download `dll` file from *Releases* section. Note: MTB-USB
drivers must be installed for this library to load: `ftd2xx.dll` must be
present on a system. MTB-USB dirvers are available in *Releases* section too.
These drivers are unsigned.

## Description

 * This library works as a middleman between MTB-USB modules and computer
   software. Is simplifies communication with the MTB.
 * Implemented in Object Pascal, last compilation and editation in Delphi 2009 32-bit.
 * Is usually exported as .dll file.
 * MTB documentation is availabe somewhere nearby this library. If not,
   send an e-mail to authors and they will send you the documentation.
 * This library fully supports just MTB-UNI & MTB-TTL modules.

See [project wiki](https://github.com/kmzbrnoI/mtb-lib/wiki) for more information.

## Authors

 * Jan Horacek (jan.horacek@kmz-brno.cz)
 * Michal Petrilak (engineercz@gmail.com)
 * Petr Travnik (petr.travnik@kmz-brno.cz)

This software is distributed as open source under Apache License v2.0.

See `<src/MTB.dpr>` for further information.
