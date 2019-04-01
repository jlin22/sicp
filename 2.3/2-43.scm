;; This makes it slow, because you try to put the new queen in rows 1-n,
;; for all board sizes, from 1-n. This makes the algorithm run in n^n time.
;; While 2-42 runs in n! time.
