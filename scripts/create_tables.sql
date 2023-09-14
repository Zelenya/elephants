CREATE TABLE product (
    id SERIAL PRIMARY KEY,
    label TEXT NOT NULL,
    description TEXT,
    UNIQUE (label)
);

CREATE TABLE category (
    id SERIAL PRIMARY KEY,
    label TEXT NOT NULL,
    UNIQUE (label)
);

CREATE TABLE product_category (
    category_id INT NOT NULL,
    product_id INT NOT NULL,
    PRIMARY KEY (category_id, product_id),
    FOREIGN KEY (product_id) REFERENCES product(id),
    FOREIGN KEY (category_id) REFERENCES category(id)
);

CREATE TABLE warehouse (
    id SERIAL PRIMARY KEY, 
    product_id INT NOT NULL,
    quantity INT NOT NULL,
    created TIMESTAMP,
    modified TIMESTAMP,
    FOREIGN KEY (product_id) REFERENCES product(id)
);
