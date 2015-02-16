/*
 *Auteur:
 *Date:
 *Description: générateur automatique de poèmes
 *
 */

import scala.io.Source
import scala.util.Random

object Main {
  def main(args:Array[String]){
    //MODIF
    val chemin_corpus:String = "C:\\Users\\yama_000\\IdeaProjects\\ScalaTP1\\src\\corpus.txt"
    //MODIF
    val chemin_dictionnaire:String = "C:\\Users\\yama_000\\IdeaProjects\\ScalaTP1\\src\\dicorimes.dmp"
    val texte = Phrases.extraire_phrases(chemin_corpus,chemin_dictionnaire)
    val poeme = new DeuxVers(texte)
    println(poeme.ecrire)
  }
}


abstract class Poeme(phrases:List[Phrase]){
  /*Renvoie des phrases aléatoirement*/
  def choose():List[Phrase] = {
    for {i<-List.range(0,phrases.length)}
    yield phrases((new Random).nextInt.abs % phrases.length)
  }

  /*Renvoie au hasard des couples de phrases qui riment*/
  def choose_deux():List[(Phrase,Phrase)] = {
    Random.shuffle(for {
      p1<-phrases
      p2<-phrases if ((p1!=p2) &&  (p1 rime_avec p2))
    } yield (p1,p2))
  }

  /*Renvoie un poème*/
  def ecrire():String
}
class DeuxVers(phrases:List[Phrase]) extends Poeme(phrases:List[Phrase]){
  /*
   * Écrire un petit poème de deux vers seulement
   * Utilisez choose_deux()
   * Faites en sorte que la différence du nombre de syllabes entre les deux vers
   * ne soit pas trop grande.
   */

  //MODIF
  def ecrire():String = choose_deux().mkString(", ")
}



//MODIF
class Mot(mot:String,nbSyllabes:Int,phonetique:String) {

  //MODIF
  override def toString():String = mot

  /*
   * Deux mots riment ssi:
   *   leurs deux dernier phones (sons) sont des voyelles identiques
   *   OU
   *   leurs deux dernier phones sont des consonnes identiques ET les deux mots, amputés de ces deux consonnent, riment
   * Cela se prête bien à du pattern matching sur les phones.  Peut-être que deux cases class Voyelle et Consonne, qui "étendent" une classe Phone seraient judicieuses à utiliser...
   * Pour celle-ci, vous avez le droit de considérer que les voyelles correspondent aux écritures phonétiques suivantes:
   * val voyelles = Set("a","e","i","o","u","y","à","è","ù","é","â","ê","î","ô","û","ä","ë","ï","ö","ü","E","§","2","5","9","8","£","@")
   */
  def rime_avec(autre_mot:Mot):Boolean = {
    val first = new Phone(this)
    val second = new Phone(autre_mot)
    if (first.est_une_voyelle && second.est_une_voyelle && first == second)
    {
      true
    }
    else
    {

    }
    false
  }
}

//MODIF
case class Phone(p:Mot) {
  protected val voyelles = Set("a","e","i","o","u","y","à","è","ù","é","â","ê","î","ô","û","ä","ë","ï","ö","ü","E","§","2","5","9","8","£","@")

  def est_une_voyelle: Boolean = voyelles contains p.toString();
}

//MODIF
//case class Voyelle() extends Phone(p:Mot) {
//}

//MODIF
//case class Consonne() extends Phone(p:Mot) {
//}


class Phrase(phrase:String,mots_hachage:Map[String,Mot]){
  /*
   * Un token est un groupe de lettre séparé par des signes de ponctuation
   * (notamment des espaces).  C'est ce qu'on appelle généralement des "mots".
   */
  private val tokens = Phrases.split_mots(phrase.toLowerCase)

  /*La liste des mots de la phrase*/
  val mots = for {
    t<-tokens
  } yield mots_hachage(t)
  override def toString():String = phrase

  /*
   * Déterminez le nombre de syllabes de la phrase.
   * Pour bien faire, utilisez map sur la liste de mots, remplacez
   * chaque mot par son nombre de syllabes et utilisez .sum sur la liste
   * qui en résulte
   */
  //MODIF
  val syllabes:Int = phrase.map(x => x).sum

  /*Deux phrases riment si le dernier mot de l'une rime avec le dernier mot de l'autre.*/
  //MODIF
  def rime_avec(phrs:Phrase):Boolean = mots(mots.length-1) rime_avec phrs.mots(phrs.mots.length-1)
}

/*Cet object compagnon permet de créer une phrase sans utiliser new Phrase(...) mais en mettant directement Phrase(...)*/
object Phrase{
  def apply(phrase:String,mots_hachage:Map[String,Mot]) = new Phrase(phrase,mots_hachage)
}


object Phrases{
  def split_mots(s:String):Array[String] =  s.trim.toLowerCase.split("[- ’—,;'()\"!.:?]+")
  def split_phrases(s:String):Array[String] = s.split("(?<=[.!?:])")
  def lire_csv(chemin:String,mots:Set[String]):List[String] ={ (for {line <- Source.fromFile(chemin).getLines()  if mots contains line.split(",")(1)} yield line).toList }

  def extraire_phrases(chemin_texte:String,chemin_dictionnaire:String):List[Phrase] = {
    val texte = Source.fromFile(chemin_texte).getLines().filter(_!="").foldLeft(""){_+_}
    /*phrases du texte*/
    val phrases_txt = for {
      phrase<-split_phrases(texte)
    } yield phrase

    /*
     * ENSEMBLE des mots du texte
     * utilisez les méthodes:
     *  split_mots
     *  toLowerCase
     *
     * ...le tout dans une "for comprehension"
     *
     * Puis utilisez .toSet sur la liste obtenue
     */
    //MODIF
    val mots_set:Set[String] = for {
      mots: String <- split_mots(texte).map(x => x.toLowerCase()).toSet
    } yield mots

    /*
     * Liste de chaines de caractères représentant chaque ligne du dictionnaire
     * On n'extrait ici que les mots qui se trouvent dans mots_set pour ne pas
     * charger tout le dictionnaire en mémoire
     */
    val dico = lire_csv(chemin_dictionnaire,mots_set)

    /*
     * Table de hachage qui contient comme clés chacun des mots du dictionnaire
     * Et comme valeur des Mot (à définir).
     * Indices:  utilisez la méthode .split(",") pour séparer les différents champs d'une ligne de dictionnaire.  Les champs intéressants
     * à conserver ici sont le 1 (le mot), le 6 (le nombre de syllabes,à convertir en entier avec .toInt) et le 8 (l'écriture phonétique)
     *         vous pouvez utiliser la méthode .toMap  de cette façon:
     *         List((key1,val1),(key2,val2)...).toMap donne Map(key1->val1,key2->val2,...)
     */
    //MODIF
    val mots_hachage:Map[String,Mot] = dico.map(i => i.split(",")(1) -> new Mot(
      i.split(",")(1),
      i.split(",")(6).toInt,
      i.split(",")(8))).toMap


    /*Liste des phrases du texte dont tous les mots font partie du dictionnaire*/
    val phrases = for {
      p<-phrases_txt  if (((split_mots(p) map (mots_hachage contains _)) forall (x=>x)) && p.trim!="")
    } yield Phrase(p.trim,mots_hachage)
    phrases.toList
  }
}