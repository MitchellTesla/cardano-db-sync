-- Persistent generated migration.

CREATE FUNCTION migrate() RETURNS void AS $$
DECLARE
  next_version int ;
BEGIN
  SELECT stage_two + 1 INTO next_version FROM schema_version ;
  IF next_version = 3 THEN
    EXECUTE 'ALTER TABLE "pool_metadata_ref" ADD COLUMN "registered_tx_id" INT8 NOT NULL' ;
    EXECUTE 'ALTER TABLE "pool_update" DROP CONSTRAINT "pool_update_meta_id_fkey"' ;
    EXECUTE 'ALTER TABLE "pool_update" ADD CONSTRAINT "pool_update_meta_id_fkey" FOREIGN KEY("meta_id") REFERENCES "pool_update"("id") ON DELETE CASCADE  ON UPDATE RESTRICT' ;
    EXECUTE 'CREATe TABLE "pool_offiline_data"("id" SERIAL8  PRIMARY KEY UNIQUE,"pool_id" INT8 NOT NULL,"ticker_name" VARCHAR NOT NULL,"hash" hash32type NOT NULL,"metadata" VARCHAR NOT NULL,"pmr_id" INT8 NULL)' ;
    EXECUTE 'ALTER TABLE "pool_offiline_data" ADD CONSTRAINT "unique_pool_offiline_data" UNIQUE("pool_id","hash")' ;
    EXECUTE 'ALTER TABLE "pool_offiline_data" ADD CONSTRAINT "pool_offiline_data_pool_id_fkey" FOREIGN KEY("pool_id") REFERENCES "pool_hash"("id") ON DELETE RESTRICT  ON UPDATE RESTRICT' ;
    EXECUTE 'ALTER TABLE "pool_offiline_data" ADD CONSTRAINT "pool_offiline_data_pmr_id_fkey" FOREIGN KEY("pmr_id") REFERENCES "pool_metadata_ref"("id") ON DELETE RESTRICT  ON UPDATE RESTRICT' ;
    EXECUTE 'CREATe TABLE "pool_offline_fetch_error"("id" SERIAL8  PRIMARY KEY UNIQUE,"fetch_time" timestamp NOT NULL,"pool_id" INT8 NOT NULL,"pmr_id" INT8 NOT NULL,"fetch_error" VARCHAR NOT NULL,"retry_count" uinteger NOT NULL)' ;
    EXECUTE 'ALTER TABLE "pool_offline_fetch_error" ADD CONSTRAINT "unique_pool_offline_fetch_error" UNIQUE("fetch_time","pool_id","retry_count")' ;
    EXECUTE 'ALTER TABLE "pool_offline_fetch_error" ADD CONSTRAINT "pool_offline_fetch_error_pool_id_fkey" FOREIGN KEY("pool_id") REFERENCES "pool_hash"("id") ON DELETE RESTRICT  ON UPDATE RESTRICT' ;
    EXECUTE 'ALTER TABLE "pool_offline_fetch_error" ADD CONSTRAINT "pool_offline_fetch_error_pmr_id_fkey" FOREIGN KEY("pmr_id") REFERENCES "pool_metadata_ref"("id") ON DELETE RESTRICT  ON UPDATE RESTRICT' ;
    -- Hand written SQL statements can be added here.
    UPDATE schema_version SET stage_two = next_version ;
    RAISE NOTICE 'DB has been migrated to stage_two version %', next_version ;
  END IF ;
END ;
$$ LANGUAGE plpgsql ;

SELECT migrate() ;

DROP FUNCTION migrate() ;
