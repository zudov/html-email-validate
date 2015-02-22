# html-email-validate

Validating an email address against HTML standard.

[![Travis build](https://travis-ci.org/zudov/html-email-validate.svg?branch=master)](https://travis-ci.org/zudov/html-email-validate) [![Hackage](https://img.shields.io/hackage/v/html-email-validate.svg)](https://hackage.haskell.org/package/html-email-validate)

The library allows to validate and parse an email address
as it's defined in [HTML standard](https://html.spec.whatwg.org/multipage/forms.html#valid-e-mail-address).

Note that HTML specification of a valid email address is a
'willful violation' of RFC 5322. If you want to validate
an address against RFC 5322 you should use [email-validate](https://hackage.haskell.org/package/email-validate).

For usage see [documentation on Hackage](https://hackage.haskell.org/package/html-email-validate/docs/Text-Html-Email-Validate.html).