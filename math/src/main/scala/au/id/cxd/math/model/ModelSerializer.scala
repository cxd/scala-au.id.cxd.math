package au.id.cxd.math.model


import java.io._

/**
  * Created by cd on 8/05/2016.
  */
object ModelSerializer {

  /**
    * serialize the model to binary file for later use.
    * When serialized in this way the model contains all state values
    * and can be used to calculate residuals and other properties.
    * as opposed to just serializing the beta parameter matrix.
    *
    * @param model
    * @param file
    */
  def serialize[T](model: T)(file: File) = {
    val oos = new ObjectOutputStream(new FileOutputStream(file))
    oos.writeObject(model)
    oos.flush()
    oos.close()
  }

  /**
    * read a binary serialized model from file for later use.
    *
    * @param file
    * @return
    */
  def deserialize[T](file: File): T = {
    val ois = new ObjectInputStream(new FileInputStream(file))
    val obj = ois.readObject().asInstanceOf[T]
    ois.close()
    obj
  }

}