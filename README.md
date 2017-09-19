[![Hackage](https://img.shields.io/hackage/v/glazier.svg)](https://hackage.haskell.org/package/glazier)
[![Build Status](https://secure.travis-ci.org/louispan/glazier.png?branch=master)](http://travis-ci.org/louispan/glazier)

# Changelog

* pre 1.0.0.0
  - Used a WindowT/GadgetT ReaderT/StateT transformer stack.
  - Combined WindowT and GadgetT into a single newtype GizmoT.
  - GizmoT contains MaybeT effects for a consistent Alternative and MonadPlus instance.

* 1.0.0.0
  - Deprecated WindowT/GadgetT/GizmoT.
  - Added 'Gate' for composing event handlers.
