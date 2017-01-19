# .emacs.d
My emacs config


It uses cask to manage various plugins used in the config.
Before you use this config, make sure you have cask [installed](http://cask.readthedocs.io/en/latest/guide/installation.html):
```
curl -fsSL https://raw.githubusercontent.com/cask/cask/master/go | python
```

Then do the following to start using this config:

```
git clone https://github.com/nitingupta910/.emacs.d.git ~/
cd ~/.emacs.d
cask upgrade-cask && cask install && cask update
```
