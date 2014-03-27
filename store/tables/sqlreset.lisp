(in-package mail-store)

(dolist (tab '(:archive_message :deliverable_message :email_address :mailbox :mailbox_has_message :message :message_id :message_reference))
  (db-base:reset-table tab))

(create-mailbox "Unassigned Mail" :description "Mail that hasn't been assigned to any object(s)." :automatically-generated t)
(create-mailbox "Recent Mail" :description "Mail that is less than 1 week old." :automatically-generated t)
;(check-mail-mbox "z:/devo/code/sellbooks/data/bookmail3")
(dotimes (i 248) (add-message 1 i))

