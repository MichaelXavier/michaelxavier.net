---
title: The ActiveRecord Hook Execution Chain
categories: activerecord,tips
---

Just thought I'd post some copypasta from the Rails documentation. Rails' documentation is massive and they hide this information where it might be hard to find tidbits like this. 

ActiveRecord executes hooks in the following order [(source)](http://api.rubyonrails.org/classes/ActiveRecord/Callbacks.html):

1.  _save called_
2.  _valid called_
3.  **before_validation hook called**
4.  **before_validation_on_create/before_validation_on_update called**
5.  _validate called_
6.  **validate_on_create validate_on_update called**
7.  **after_validation called**
8.  **after_validation_on_create/after_validation_on_update called**
9.  **before_save called**
10. **before_create/before_update called**
11. _create/update (at last)_
12. **after_create/after_update called**
13. **after_save called**