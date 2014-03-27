(in-package cl-user)
(mapcar #'db-base:load-sql (directory (merge-pathnames #P(:name "*" :type "SQL") *load-pathname*)))
