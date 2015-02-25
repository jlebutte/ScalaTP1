/*
 *Auteur: Julien Lebutte
 *Date: 19/02/2015
 *Description: générateur automatique de poèmes
 *
 */

import scala.io.Source
import scala.util.{Try, Random, Success, Failure}

object Main {
  def main(args:Array[String]){
    val chemin_corpus:String = "C:\\Users\\yama_000\\IdeaProjects\\ScalaTP1\\src\\corpus.txt"
    val chemin_dictionnaire:String = "C:\\Users\\yama_000\\IdeaProjects\\ScalaTP1\\src\\dicorimes.dmp"
    val poeme = for {
      texte<- Phrases.extraire_phrases(chemin_corpus,chemin_dictionnaire)
    } yield new DeuxVers(texte)
    poeme match {
      case Success(x) => println(x.ecrire())
      case Failure(f) => println("Erreur : " + f)
    }
  }
}


abstract class Poeme(phrases:List[Phrase]){
  /*Renvoie des phrases aléatoirement*/
  def choose():List[Phrase] = {
    for {i<-List.range(0,phrases.length)}
    yield phrases((new Random).nextInt.abs % phrases.length)
  }


  val ints = new Generator[Int] {
    val rand = new java.util.Random
    def generate = rand.nextInt()
  }

  //Generateur aleatoire de phrases
  private val phrases_aleatoires =  new Generator[Phrase] {
    def generate = phrases(ints.generate.abs % phrases.length)
  }

  private def phrases_generator(ph:List[Phrase]):Generator[List[Phrase]] = {
    new Generator[List[Phrase]] {
      def generate = for {i<-List.range(0,ph.length)}
      yield ph(ints.generate.abs % ph.length)}
  }

  def filterRime(p:Phrase): List[Phrase] = {
    phrases.filter(x => x rime_avec p)
  }

  //Generateur aleatoire de couples de phrases qui riment
  val couple_riment:Option[Generator[(Phrase,Phrase)]]= {
    if (!phrases.exists(p1 => phrases.exists(p2 => (p1 rime_avec p2) && p1 != p2 && Math.abs(p1.syllabes - p2.syllabes).<(3)))) None
    else {
      val filterList = phrases_aleatoires.filter(p1 => phrases.exists(p2 => (p1 rime_avec p2) && p1 != p2 && Math.abs(p1.syllabes - p2.syllabes).<(3)))
      Some(filterList flatMap(p1 => for(p2 <- filterList.filter(p2 => (p2 rime_avec p1) && p1 != p2 && Math.abs(p1.syllabes - p2.syllabes).<(3))) yield (p1, p2)))
    }
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

  // Vérification du nombre de syllabes à l'aide une fonction.
  // Si les deux phrases ont plus de 2 syllabes d'écart, on rejette
  def ecrire():String = {
    couple_riment match {
      case Some(x) => {val gen = x.generate
        gen._1.toString() + "\n" + gen._2.toString()}
      case None => "Il n'existe pas de couples possibles"
    }
  }
}


class Mot(mot:String,nbSyllabes:Int,phonetique:String) {

  override def toString():String = mot
  def countSyllabes():Int = nbSyllabes
  def phonetiquetoString():String = phonetique

  /*
   * Deux mots riment ssi:
   *   leurs deux dernier phones (sons) sont des voyelles identiques
   *   OU
   *   leurs deux dernier phones sont des consonnes identiques ET les deux mots, amputés de ces deux consonnent, riment
   * Cela se prête bien à du pattern matching sur les phones.  Peut-être que deux cases class Voyelle et Consonne, qui "étendent" une classe Phone seraient judicieuses à utiliser...
   * Pour celle-ci, vous avez le droit de considérer que les voyelles correspondent aux écritures phonétiques suivantes:
   * val voyelles = Set("a","e","i","o","u","y","à","è","ù","é","â","ê","î","ô","û","ä","ë","ï","ö","ü","E","§","2","5","9","8","£","@")
   */

  /*
  Pattern matching sur les phones + variables non-mutables
  First = dernière syllabes du mot
  Second = dernière syllabe de autre_mot

  La pattern matching permet de distinguer les différents cas possibles pour la comparaison selon les commentaires ci-dessus
  S'il s'agit de deux consonnes identiquent, on appelle de nouveau la fonction en retirant la dernière syllabe du mot phonétique.
  */
  def rime_avec(autre_mot:Mot):Boolean = {
    val voyelles = Set("a","e","i","o","u","y","à","è","ù","é","â","ê","î","ô","û","ä","ë","ï","ö","ü","E","§","2","5","9","8","£","@")
    val first = if(voyelles contains phonetique.last.toString) new Voyelle(phonetique.last) else new Consonne(phonetique.last)
    val second = if(voyelles contains autre_mot.phonetiquetoString.last.toString) new Voyelle(autre_mot.phonetiquetoString.last) else new Consonne(autre_mot.phonetiquetoString.last)
    (first, second) match {
        case (first: Voyelle, second: Voyelle) =>
            first.getChar == second.getChar
        case (first: Consonne, second: Consonne) =>
          if(first == second) {
            val newMot = new Mot(mot.dropRight(1), nbSyllabes - 1, phonetique.dropRight(1))
            newMot.rime_avec(new Mot(autre_mot.toString().dropRight(1), autre_mot.countSyllabes() - 1, autre_mot.phonetiquetoString().dropRight(1)))
          }
          else false
        case _ => false
        }
    }
}

// Création d'une class Phone et utilisation de l'héritage
// Les case class Voyelle et Consonne sont utilisées pour le pattern matching
class Phone(p: Char) {
  def getChar:Char = p
}

case class Voyelle(v: Char) extends Phone(p = v) {

}

case class Consonne(c: Char) extends Phone(p = c) {

}


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

  // Utilisation de la fonction de haut niveau map
  // Utilisation des fonctions anonymes
  val syllabes:Int = mots.map(x => x.countSyllabes).sum

  /*Deux phrases riment si le dernier mot de l'une rime avec le dernier mot de l'autre.*/
  def rime_avec(phrs:Phrase):Boolean = mots.last rime_avec phrs.mots.last
}

/*Cet object compagnon permet de créer une phrase sans utiliser new Phrase(...) mais en mettant directement Phrase(...)*/
object Phrase{
  def apply(phrase:String,mots_hachage:Map[String,Mot]) = new Phrase(phrase,mots_hachage)
}


object Phrases{
  def split_mots(s:String):Array[String] =  s.trim.toLowerCase.split("[- ’—,;'()\"!.:?]+")
  def split_phrases(s:String):Array[String] = s.split("(?<=[.!?:])")
  def lire_csv(chemin:String,mots:Set[String]):List[String] ={ (for {line <- Source.fromFile(chemin).getLines()  if mots contains line.split(",")(1)} yield line).toList }

  def extraire_phrases(chemin_texte:String,chemin_dictionnaire:String):Try[List[Phrase]] = {
    for {
        texte <- Try(Source.fromFile(chemin_texte).getLines().filter(_!="").foldLeft(""){_+_})
        phrases_txt = split_phrases(texte)
        mots_set = texte.trim.split("[- ’—,;'()\"!.:?]+").map(x => x.toLowerCase).toSet
        dico <- Try(Source.fromFile(chemin_dictionnaire).getLines().filter(x => mots_set contains x.split(",")(1)).toList)
        mots_hachage = dico.map(i => i.split(",")(1) -> new Mot(
          i.split(",")(1),
          i.split(",")(6).toInt,
          i.split(",")(8))).toMap
        phrases = phrases_txt.filter(p => ((split_mots(p) map (mots_hachage contains _)) forall (x=>x)) && p.trim!="").map(p => Phrase(p.trim,mots_hachage)).toList
    } yield phrases
  }
}

trait Generator[T] {
  self =>
  def generate : T
  def map[S](f: T => S): Generator[S] = new Generator[S] {
    def generate = f(self.generate)
  }
  def flatMap[S](f: T => Generator[S]): Generator[S] =
    new Generator[S] {
      def generate = f(self.generate).generate
    }
  def	filter (f : T => Boolean) : Generator[T] = new Generator[T]  {
    override def generate =
    {
      val gen = self.generate
      if (f(gen)) gen else this.generate
    }
  }
}