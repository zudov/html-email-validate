# html-email-validate

Validating an email address against HTML standard.

[![Travis][travis-badge]][travis-page]

[![Hackage][hackage-badge]][hackage-page]
[![Stackage LTS][stackage-lts-badge]][stackage-lts-page]
[![Stackage Nightly][stackage-nightly-badge]][stackage-nightly-page]

[travis-badge]: https://travis-ci.org/zudov/html-email-validate.svg?branch=master
[travis-page]: https://travis-ci.org/zudov/html-email-validate

[hackage-badge]: https://img.shields.io/hackage/v/html-email-validate.svg
[hackage-page]: https://hackage.haskell.org/package/html-email-validate

[stackage-lts-badge]: https://www.stackage.org/package/html-email-validate/badge/lts
[stackage-lts-page]: https://www.stackage.org/lts/package/html-email-validate

[stackage-nightly-badge]: https://www.stackage.org/package/html-email-validate/badge/nightly
[stackage-nightly-page]: https://www.stackage.org/nightly/package/html-email-validate

The library allows to validate and parse an email address
as it's defined in [HTML standard](https://html.spec.whatwg.org/multipage/forms.html#valid-e-mail-address).

Note that HTML specification of a valid email address is a
'willful violation' of RFC 5322. If you want to validate
an address against RFC 5322 you should use [email-validate](https://hackage.haskell.org/package/email-validate).

## Usage

```haskell

Prelude> import Text.Html.Email.Validate 
Prelude Text.Html.Email.Validate> :set -XOverloadedStrings 
Prelude Text.Html.Email.Validate> isValidEmail "mailto@@mail.to"
False
Prelude Text.Html.Email.Validate> parseEmail "mailto@mail.to"
Right mailto@mail.to

```

For more examples see [documentation on Hackage](https://hackage.haskell.org/package/html-email-validate/docs/Text-Html-Email-Validate.html).
