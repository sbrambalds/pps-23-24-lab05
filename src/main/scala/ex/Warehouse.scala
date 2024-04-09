package ex

import ex.Item.unapply
import util.Optionals.Optional
import util.Sequences.Sequence
trait Item:
  def code: Int
  def name: String
  def tags: Seq[String]

object Item:
  def apply(code: Int, name: String, tags: String*): Item = ItemImpl(code, name, tags)

  def unapply(item: Item): Option[(Int, String, Seq[String])] = Some(item.code, item.name, item.tags)

  private case class ItemImpl(override val code: Int, override val name: String, override val tags: Seq[String]) extends Item:
    override def toString: String = name

/**
 * A warehouse is a place where items are stored.
 */
trait Warehouse:
  /**
   * Stores an item in the warehouse.
   * @param item the item to store
   */
  def store(item: Item): Unit
  /**
   * Searches for items with the given tag.
   * @param tag the tag to search for
   * @return the list of items with the given tag
   */
  def searchItems(tag: String): Sequence[Item]
  /**
   * Retrieves an item from the warehouse.
   * @param code the code of the item to retrieve
   * @return the item with the given code, if present
   */
  def retrieve(code: Int): Optional[Item]
  /**
   * Removes an item from the warehouse.
   * @param item the item to remove
   */
  def remove(item: Item): Unit
  /**
   * Checks if the warehouse contains an item with the given code.
   * @param itemCode the code of the item to check
   * @return true if the warehouse contains an item with the given code, false otherwise
   */
  def contains(itemCode: Int): Boolean

end Warehouse

object Warehouse:
  def apply(): Warehouse = WarehouseImpl(Sequence.empty)

//  def someTag(seq: Sequence[Item]): Option[String] =
//
//    def _takeTag(): String = seq match
//      case Sequence.Cons(item, t) => item match
//        case Item(_, _, value) =>
//          for tag <- value do 
//            if searchItems()

  private case class WarehouseImpl(private var items: Sequence[Item]) extends Warehouse:
    /**
     * Stores an item in the warehouse.
     *
     * @param item the item to store
     */
    override def store(item: Item): Unit =
      items = items.add(item)

    /**
     * Searches for items with the given tag.
     *
     * @param tag the tag to search for
     * @return the list of items with the given tag
     */
    override def searchItems(tag: String): Sequence[Item] =
      items.filter(v => v.tags.contains(tag))

    /**
     * Retrieves an item from the warehouse.
     *
     * @param code the code of the item to retrieve
     * @return the item with the given code, if present
     */
    override def retrieve(code: Int): Optional[Item] =
      items.find(v => v.code == code)

    /**
     * Removes an item from the warehouse.
     *
     * @param item the item to remove
     */
    override def remove(item: Item): Unit =
      items = items.filter(v => v != item)

    /**
     * Checks if the warehouse contains an item with the given code.
     *
     * @param itemCode the code of the item to check
     * @return true if the warehouse contains an item with the given code, false otherwise
     */
    override def contains(itemCode: Int): Boolean = items.filter(v => v.code == itemCode) != Sequence.empty

    override def toString: String = items.toString

    extension(seq: Sequence[Item])
      def sameTags(): Option[Sequence[String]] = ???


@main def mainWarehouse(): Unit =
  val warehouse = Warehouse()

  val dellXps = Item(33, "Dell XPS 15", "notebook")
  val dellInspiron = Item(34, "Dell Inspiron 13", "notebook")
  val xiaomiMoped = Item(35, "Xiaomi S1", "moped", "mobility")

  println(warehouse.contains(dellXps.code)) // false
  warehouse.store(dellXps) // side effect, add dell xps to the warehouse
  println(warehouse.contains(dellXps.code)) // true
  warehouse.store(dellInspiron) // side effect, add dell Inspiron to the warehouse
  warehouse.store(xiaomiMoped) // side effect, add xiaomi moped to the warehouse
  println(warehouse.searchItems("mobility")) // Sequence(xiaomiMoped)
  println(warehouse.searchItems("notebook")) // Sequence(dellXps, dell Inspiron)
  println(warehouse.retrieve(11)) // None
  println(warehouse.retrieve(dellXps.code)) // Just(dellXps)
  warehouse.remove(dellXps) // side effect, remove dell xps from the warehouse
  println(warehouse.retrieve(dellXps.code)) // None

/** Hints:
 * - Implement the Item with a simple case class
 * - Implement the Warehouse keeping a private Sequence of items
 * - Start implementing contains and store
 * - Implement searchItems using filter and contains
 * - Implement retrieve using find
 * - Implement remove using filter
 * - Refactor the code of Item accepting a variable number of tags (hint: use _*)
*/