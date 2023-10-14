CREATE TABLE graph (
    id serial PRIMARY KEY,
    name VARCHAR (255) UNIQUE NOT NULL,
    graph_string TEXT
);
