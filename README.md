## Port to MongoDB ##

This is a fork of [Pocketchange] to show:

 * how the model can be made persistance-free (almost), i.e. using plain scala objects
 * how MongoDB can be used as a persistance backend using mongo-scala-driver: all the queries to DB that where using Mapper are replaced by queries to MongoDB

The app is not using Mapper at all and all the functionality (including receipt image storage and custom BigDecimal field) is implemented using mongo-scala-driver.

  [Pocketchange]: http://github.com/tjweir/pocketchangeapp

## Original README ##
This is the demo application that we're building to support the Lift Book.
http://www.github.com/tjweir/liftbook/


- Derek, Marius and Tyler
