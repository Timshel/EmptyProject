
# === !Ups


CREATE TABLE ANORM_DATE (
    id                          INTEGER                 NOT NULL AUTO_INCREMENT,
    date_debut                  DATE                    ,
    date_fin                    DATE                    ,
    date_modif                  DATETIME                ,

    PRIMARY KEY ( id )
);

# === !Downs

DROP TABLE ANORM_DATE;