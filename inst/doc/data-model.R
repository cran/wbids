## erDiagram
##     ENTITIES ||--o{ DEBT_STATISTICS : has
##     ENTITIES {
##         string entity_id PK
##         string entity_name
##         string entity_iso2code
##         string entity_type
##         string capital_city
##         string region_id
##         string region_iso2code
##         string region_name
##         string admin_region_id
##         string admin_region_iso2code
##         string admin_region_name
##         string lending_type_id
##         string lending_type_iso2code
##         string lending_type_name
##     }
##     SERIES ||--o{ DEBT_STATISTICS : has
##     SERIES ||--o{ SERIES_TOPICS : has
##     SERIES {
##         string series_id PK
##         string series_name
##         int source_id
##         string source_note
##         string source_organization
##     }
##     COUNTERPARTS ||--o{ DEBT_STATISTICS : has
##     COUNTERPARTS {
##         string counterpart_id PK
##         string counterpart_name
##         string counterpart_iso2code
##         string counterpart_iso3code
##         string counterpart_type
##     }
##     SERIES_TOPICS {
##         string series_id FK
##         int topic_id
##         string topic_name
##     }
##     DEBT_STATISTICS {
##         string series_id FK
##         string entity_id FK
##         string counterpart_id FK
##         int year
##         float value
##     }
