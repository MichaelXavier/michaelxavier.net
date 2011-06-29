---
title: Broken Audio in Flash on Chrome for Ubuntu
categories: tips,ubuntu,chrome
---

I can hardly stand to use Firefox anymore. Lately I've only used it for pages that are Chrome bugged or because flash audio wasn't working. Here's how I fixed it.

Firefox is all but dead to me. It's slow as a dog, takes at least 4 times as long to load as chrome, about 4 times as long to close and is way too easy to crash. Despite being beta software, Chrome has been extremely stable for me. If a tab gets out of control, it can easily be killed off without affecting the other pages. This is what Firefox should have been doing a long time ago.

Anyways, audio wasn't working for me for the longest time but I found a fix on the google support pages from a poster. It worked for me. I'll bet with this I can get myself down to opening firefox maybe once or twice a week!

~~~~{.sh}
sudo mkdir /opt/google/chrome/plugins
sudo cp /usr/lib/mozilla/plugins/flashplugin-alternative.so /opt/google/chrome/plugins/libflashplayer.so
~~~~

To-do list for Chrome Devs:

1. AdBlock Plus clone (yes I know about AdSweeper, it's terrible).
2. Fix Acrobat Reader not opening PDFs properly in the browser.
3. Multiple tab rows?
4. Better bookmark system.

I'm still kind of amazed at how quickly Google was able to develop a browser that was far and away better than Firefox at *browsing*. Yes, I realize that it runs Webkit and that Webkit is part of Safari, but I will not have anyone sit there and tell me that Safari is good browser because it isn't. Yes, I realize Firefox has all the best plugins, but in the end, the quality of the browser is far more important than the extensions people make for it.
