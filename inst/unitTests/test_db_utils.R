test_db_connections <- function(){

   hub <-  AnnotationHub()
   path <- hub@.db_path

   # test dbfile
   checkIdentical(path, dbfile(hub))

   conn <- dbconn(AnnotationHub())
   conn2 <-  AnnotationDbi::dbFileConnect(path)

   # test dbconn
   checkTrue(RSQLite::dbIsValid(conn))
   checkTrue(RSQLite::dbIsValid(conn2))
   checkIdentical(DBI::dbListTables(conn), DBI::dbListTables(conn2))

   DBI::dbDisconnect(conn)
   AnnotationHub:::.db_close(conn2)

   # test .db_uid0
   dates <- possibleDates(hub)
   ids1 <- AnnotationHub:::.db_uid0(path, dates[1])
   ids2 <- AnnotationHub:::.db_uid0(path, dates[length(dates)])
   # assumes more resources added than removed
   checkTrue(length(ids2) > length(ids1))

   # test .db_id
   checkIdentical(ids2,  AnnotationHub:::.db_uid(hub))

   # test index file
   indexfile <- AnnotationHub:::.db_index(hub)
   time1 <- file.mtime(indexfile)
   # this shouldn't recreate but just return existing
   indexfile2 <- AnnotationHub:::.db_create_index(hub)
   checkIdentical(indexfile, indexfile2)
   checkIdentical(time1, file.mtime(indexfile2))
   # same should just return
   indexfile3 <- AnnotationHub:::.db_index_file(hub)
   checkIdentical(unname(indexfile), indexfile3)
   checkIdentical(time1, file.mtime(indexfile3))
   index1 <- AnnotationHub:::.db_index_load(hub)
   index2 <- readRDS(indexfile)
   checkIdentical(index1, index2)

}
