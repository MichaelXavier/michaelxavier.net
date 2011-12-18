---
title: RememberTheMilk URL Task Bookmarklet
categories: rememberthemilk
---

RememberTheMilk offers a bookmarklet to quickly add a task based on the page of you are currently on. For whatever reason, they do not fill in the URL for the task, only using the title of the page as the title of the task. Digging into the bookmarklet code, this was really simple to fix.

Drag the box below into your bookmark bar to install the bookarklet.

[Add to RTM!](javascript:(function(){h='www.rememberthemilk.com';p='/services/ext/addtask.rtm';if(window.getSelection){d=window.getSelection();}else if(document.getSelection){d=document.getSelection();}else if(document.selection){d=document.selection.createRange().text;};cp='http://'+h+p+'?d='+encodeURIComponent(d)+'&t='+encodeURIComponent(document.title + " " + window.location);w=window.open(cp,'addwindow','status=no,toolbar=no,width=475,height=260,resizable=yes');setTimeout(function(){w.focus();}, 500);})();)
